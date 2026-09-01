(function () {
  "use strict";

  var SVG_NS = "http://www.w3.org/2000/svg";
  var GROUP_COLORS = { high: "#4C5FFF", low: "#FF4D4D" };
  var WAVE_COLORS = {
    initial: "#FF9999",
    alpha: "#99CC99",
    delta: "#9999FF",
    omicron: "#FFFF66"
  };
  var AGE_ORDER = [
    "0-44", "20-39", "40-59", "45-64", "60-79", "65-84", "GE80", "GE85"
  ];
  var numberFormatter = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 2
  });

  function query(root, selector) {
    return root.querySelector(selector);
  }

  function clear(element) {
    while (element.firstChild) element.removeChild(element.firstChild);
  }

  function finite(value) {
    return value !== null && value !== "" && Number.isFinite(Number(value));
  }

  function svgElement(name, attributes) {
    var element = document.createElementNS(SVG_NS, name);
    Object.keys(attributes || {}).forEach(function (key) {
      element.setAttribute(key, attributes[key]);
    });
    return element;
  }

  function rootPath(app, relative) {
    return app.dataset.root.replace(/\/$/, "") + "/" + relative;
  }

  async function fetchJSON(path) {
    var response = await fetch(path, { cache: "no-cache" });
    if (!response.ok) {
      throw new Error("Unable to load " + path + " (HTTP " + response.status + ").");
    }
    return response.json();
  }

  function setOptions(select, options, preferred) {
    clear(select);
    options.forEach(function (option) {
      var node = document.createElement("option");
      node.value = option.value;
      node.textContent = option.label;
      select.appendChild(node);
    });
    var values = options.map(function (option) { return option.value; });
    if (preferred && values.indexOf(preferred) !== -1) {
      select.value = preferred;
    } else if (options.length > 0) {
      select.value = options[0].value;
    }
  }

  function thresholdGroup(rate, lowThreshold, highThreshold) {
    if (Number(rate) < lowThreshold) return "low";
    if (Number(rate) > highThreshold) return "high";
    return "excluded";
  }

  function panelKey(figure, region, ageGroup) {
    return figure + "::" + region + "::" + ageGroup;
  }

  function formatPercent(value) {
    if (!finite(value)) return "Not available";
    var percent = Number(value) * 100;
    var digits = Math.abs(percent) < 10 ? 1 : 0;
    return percent.toFixed(digits) + "%";
  }

  function formatRate(value) {
    return finite(value) ? Number(value).toFixed(2) : "Not available";
  }

  function csvValue(value) {
    if (value === null || typeof value === "undefined") return "";
    var text = String(value);
    return /[",\n]/.test(text) ? '"' + text.replace(/"/g, '""') + '"' : text;
  }

  function downloadCSV(filename, rows, columns) {
    var lines = [columns.join(",")];
    rows.forEach(function (row) {
      lines.push(columns.map(function (column) {
        return csvValue(row[column]);
      }).join(","));
    });
    var blob = new Blob([lines.join("\n") + "\n"], {
      type: "text/csv;charset=utf-8"
    });
    var url = URL.createObjectURL(blob);
    var link = document.createElement("a");
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(url);
  }

  function linePath(rows, column, xForTime, yForValue) {
    var path = "";
    var active = false;
    rows.forEach(function (row) {
      if (!finite(row[column])) {
        active = false;
        return;
      }
      var x = xForTime(new Date(row.date + "T00:00:00Z").getTime());
      var y = yForValue(Number(row[column]));
      path += (active ? "L" : "M") + x.toFixed(2) + "," + y.toFixed(2);
      active = true;
    });
    return path;
  }

  function areaPath(rows, xForTime, yForValue) {
    var usable = rows.filter(function (row) {
      return finite(row.lower) && finite(row.upper);
    });
    if (usable.length === 0) return "";
    var upper = usable.map(function (row, index) {
      return (index === 0 ? "M" : "L") +
        xForTime(new Date(row.date + "T00:00:00Z").getTime()).toFixed(2) + "," +
        yForValue(Number(row.upper)).toFixed(2);
    }).join("");
    var lower = usable.slice().reverse().map(function (row) {
      return "L" +
        xForTime(new Date(row.date + "T00:00:00Z").getTime()).toFixed(2) + "," +
        yForValue(Number(row.lower)).toFixed(2);
    }).join("");
    return upper + lower + "Z";
  }

  function renderNoData(svg, message) {
    clear(svg);
    var label = svgElement("text", { x: 460, y: 250, class: "vax-no-data" });
    label.textContent = message;
    svg.appendChild(label);
  }

  function renderWaveBands(svg, waves, minDate, maxDate, xForTime, margin, plotHeight) {
    (waves || []).forEach(function (wave) {
      var start = new Date(wave.start + "T00:00:00Z").getTime();
      var end = new Date(wave.end_exclusive + "T00:00:00Z").getTime();
      var visibleStart = Math.max(start, minDate);
      var visibleEnd = Math.min(end, maxDate);
      if (visibleStart >= visibleEnd) return;
      var color = WAVE_COLORS[wave.wave] || "#cccccc";
      svg.appendChild(svgElement("rect", {
        x: xForTime(visibleStart),
        y: margin.top,
        width: Math.max(0, xForTime(visibleEnd) - xForTime(visibleStart)),
        height: plotHeight,
        fill: color,
        "fill-opacity": 0.12
      }));
      svg.appendChild(svgElement("line", {
        x1: xForTime(visibleStart),
        y1: margin.top,
        x2: xForTime(visibleStart),
        y2: margin.top + plotHeight,
        stroke: color,
        class: "vax-wave-boundary"
      }));
      var label = svgElement("text", {
        x: (xForTime(visibleStart) + xForTime(visibleEnd)) / 2,
        y: margin.top + 15,
        "text-anchor": "middle",
        class: "vax-wave-label"
      });
      label.textContent = wave.wave.charAt(0).toUpperCase() + wave.wave.slice(1);
      svg.appendChild(label);
    });
  }

  function aggregateFixedEffect(shard, assignments, vaccinationGroup) {
    var selected = shard.series.filter(function (series) {
      return assignments[series.geography] === vaccinationGroup;
    });
    if (selected.length === 0) return [];

    var byDate = {};
    selected.forEach(function (series) {
      series.date.forEach(function (date, index) {
        if (!byDate[date]) byDate[date] = [];
        byDate[date].push({
          geography: series.geography,
          mean: series.mean[index],
          variance: series.variance[index]
        });
      });
    });

    return Object.keys(byDate).sort().map(function (date) {
      var rows = byDate[date];
      if (rows.length !== selected.length) return null;
      var usable = rows.filter(function (row) {
        return finite(row.mean) && finite(row.variance) && Number(row.variance) > 0;
      });
      if (usable.length === 0) return null;
      var denominator = usable.reduce(function (sum, row) {
        return sum + 1 / Number(row.variance);
      }, 0);
      var weightedMean = usable.reduce(function (sum, row) {
        return sum + Number(row.mean) / Number(row.variance);
      }, 0) / denominator;
      var variance = 1 / denominator;
      return {
        vaccination_group: vaccinationGroup,
        date: date,
        mean: weightedMean,
        variance: variance,
        lower: weightedMean - 1.96 * Math.sqrt(variance),
        upper: weightedMean + 1.96 * Math.sqrt(variance),
        jurisdictions: selected.length,
        contributing_jurisdictions: usable.length,
        interval_method: "fixed_effect_normal_approximation"
      };
    }).filter(function (row) { return row !== null; });
  }

  function smoothCenteredBox(rows, bandwidthDays) {
    var halfWidthMilliseconds = bandwidthDays / 2 * 24 * 60 * 60 * 1000;
    return rows.map(function (row) {
      var target = new Date(row.date + "T00:00:00Z").getTime();
      var neighbors = rows.filter(function (candidate) {
        var timestamp = new Date(candidate.date + "T00:00:00Z").getTime();
        return Math.abs(timestamp - target) <= halfWidthMilliseconds;
      });
      var mean = neighbors.reduce(function (sum, candidate) {
        return sum + Number(candidate.mean);
      }, 0) / neighbors.length;
      var variance = neighbors.reduce(function (sum, candidate) {
        return sum + Number(candidate.variance);
      }, 0) / neighbors.length;
      return {
        vaccination_group: row.vaccination_group,
        date: row.date,
        mean: mean,
        variance: variance,
        lower: mean - 1.96 * Math.sqrt(variance),
        upper: mean + 1.96 * Math.sqrt(variance),
        jurisdictions: row.jurisdictions,
        contributing_jurisdictions: row.contributing_jurisdictions,
        interval_method: row.interval_method + "_centered_14_day_box_smoothed"
      };
    });
  }

  function filterWindow(rows, windowValue) {
    if (windowValue === "full") return rows.slice();
    var firstDate = windowValue + "-01-01";
    return rows.filter(function (row) { return row.date >= firstDate; });
  }

  function addTooltipLines(tooltip, heading, lines) {
    clear(tooltip);
    var title = document.createElement("strong");
    title.textContent = heading;
    tooltip.appendChild(title);
    lines.forEach(function (line) {
      var span = document.createElement("span");
      span.textContent = line;
      tooltip.appendChild(span);
    });
  }

  function renderChart(app, index, figure, windowValue, groupedRows) {
    var svg = query(app, '[data-role="chart"]');
    var tooltip = query(app, '[data-role="tooltip"]');
    clear(svg);
    tooltip.hidden = true;
    var displayed = {
      high: filterWindow(groupedRows.high, windowValue),
      low: filterWindow(groupedRows.low, windowValue)
    };
    var combined = displayed.high.concat(displayed.low);
    if (combined.length === 0) {
      renderNoData(svg, "Assign at least one available location to Low or High.");
      return;
    }

    var timestamps = combined.map(function (row) {
      return new Date(row.date + "T00:00:00Z").getTime();
    });
    var candidates = [];
    combined.forEach(function (row) {
      [row.lower, row.mean, row.upper].forEach(function (value) {
        if (finite(value)) candidates.push(Number(value));
      });
    });
    var width = 920;
    var height = 500;
    var margin = { top: 44, right: 25, bottom: 60, left: 82 };
    var plotWidth = width - margin.left - margin.right;
    var plotHeight = height - margin.top - margin.bottom;
    var minDate = Math.min.apply(null, timestamps);
    var maxDate = Math.max.apply(null, timestamps);
    var yMin = Math.min.apply(null, candidates.concat([0]));
    var yMax = Math.max.apply(null, candidates.concat([0]));
    if (yMin === yMax) {
      yMin -= 0.05;
      yMax += 0.05;
    }
    var yPadding = (yMax - yMin) * 0.08;
    yMin -= yPadding;
    yMax += yPadding;

    function xForTime(timestamp) {
      return margin.left + (timestamp - minDate) /
        Math.max(1, maxDate - minDate) * plotWidth;
    }
    function yForValue(value) {
      return margin.top + (yMax - value) / (yMax - yMin) * plotHeight;
    }

    renderWaveBands(svg, index.waves, minDate, maxDate, xForTime, margin, plotHeight);

    var yTicks = 5;
    for (var yTick = 0; yTick <= yTicks; yTick += 1) {
      var yValue = yMin + (yMax - yMin) * yTick / yTicks;
      var y = yForValue(yValue);
      svg.appendChild(svgElement("line", {
        x1: margin.left,
        y1: y,
        x2: width - margin.right,
        y2: y,
        class: "vax-grid-line"
      }));
      var yLabel = svgElement("text", {
        x: margin.left - 10,
        y: y + 4,
        "text-anchor": "end",
        class: "vax-grid-label"
      });
      yLabel.textContent = formatPercent(yValue);
      svg.appendChild(yLabel);
    }

    var xTicks = 5;
    for (var xTick = 0; xTick <= xTicks; xTick += 1) {
      var timestamp = minDate + (maxDate - minDate) * xTick / xTicks;
      var x = xForTime(timestamp);
      svg.appendChild(svgElement("line", {
        x1: x,
        y1: height - margin.bottom,
        x2: x,
        y2: height - margin.bottom + 6,
        class: "vax-axis"
      }));
      var xLabel = svgElement("text", {
        x: x,
        y: height - margin.bottom + 24,
        "text-anchor": "middle",
        class: "vax-grid-label"
      });
      var tickDate = new Date(timestamp);
      var spanYears = (maxDate - minDate) / (365.25 * 24 * 60 * 60 * 1000);
      xLabel.textContent = spanYears <= 8 ?
        tickDate.getUTCFullYear() + "-" +
          String(tickDate.getUTCMonth() + 1).padStart(2, "0") :
        String(tickDate.getUTCFullYear());
      svg.appendChild(xLabel);
    }

    svg.appendChild(svgElement("line", {
      x1: margin.left,
      y1: margin.top,
      x2: margin.left,
      y2: height - margin.bottom,
      class: "vax-axis"
    }));
    svg.appendChild(svgElement("line", {
      x1: margin.left,
      y1: height - margin.bottom,
      x2: width - margin.right,
      y2: height - margin.bottom,
      class: "vax-axis"
    }));
    if (yMin <= 0 && yMax >= 0) {
      svg.appendChild(svgElement("line", {
        x1: margin.left,
        y1: yForValue(0),
        x2: width - margin.right,
        y2: yForValue(0),
        class: "vax-reference-line"
      }));
    }

    ["high", "low"].forEach(function (group) {
      var rows = displayed[group];
      if (rows.length === 0) return;
      var color = GROUP_COLORS[group];
      if (figure === "figure_04") {
        var ribbon = areaPath(rows, xForTime, yForValue);
        if (ribbon) {
          svg.appendChild(svgElement("path", {
            d: ribbon,
            fill: color,
            class: "vax-interval"
          }));
        }
      }
      svg.appendChild(svgElement("path", {
        d: linePath(rows, "mean", xForTime, yForValue),
        stroke: color,
        class: "vax-group-line"
      }));
      if (figure === "figure_05") {
        ["lower", "upper"].forEach(function (column) {
          svg.appendChild(svgElement("path", {
            d: linePath(rows, column, xForTime, yForValue),
            stroke: color,
            class: "vax-bound-line"
          }));
        });
      }
    });

    var yAxisLabel = svgElement("text", {
      x: 18,
      y: margin.top + plotHeight / 2,
      transform: "rotate(-90 18 " + (margin.top + plotHeight / 2) + ")",
      "text-anchor": "middle",
      class: "vax-axis-label"
    });
    yAxisLabel.textContent = figure === "figure_04" ?
      "P-score" : "P-score difference (female − male)";
    svg.appendChild(yAxisLabel);

    var legend = svgElement("g", { transform: "translate(" + margin.left + " 20)" });
    var legendX = 0;
    ["high", "low"].forEach(function (group) {
      legend.appendChild(svgElement("line", {
        x1: legendX,
        y1: 0,
        x2: legendX + 22,
        y2: 0,
        stroke: GROUP_COLORS[group],
        "stroke-width": 3
      }));
      var label = svgElement("text", {
        x: legendX + 28,
        y: 4,
        class: "vax-grid-label"
      });
      label.textContent = group.charAt(0).toUpperCase() + group.slice(1) + " vaccination";
      legend.appendChild(label);
      legendX += 150;
    });
    var intervalLabel = svgElement("text", {
      x: legendX,
      y: 4,
      class: "vax-grid-label"
    });
    intervalLabel.textContent = figure === "figure_04" ?
      "Shaded: 95% interval" : "Dashed: 95% interval";
    legend.appendChild(intervalLabel);
    svg.appendChild(legend);

    var focusLine = svgElement("line", {
      x1: margin.left,
      y1: margin.top,
      x2: margin.left,
      y2: height - margin.bottom,
      class: "vax-focus-line",
      visibility: "hidden"
    });
    svg.appendChild(focusLine);
    var overlay = svgElement("rect", {
      x: margin.left,
      y: margin.top,
      width: plotWidth,
      height: plotHeight,
      fill: "transparent",
      tabindex: 0,
      "aria-label": "Move across the chart to inspect grouped estimates"
    });
    svg.appendChild(overlay);
    var uniqueDates = Array.from(new Set(combined.map(function (row) {
      return row.date;
    }))).sort();

    function showAt(date, clientX, clientY) {
      var timestamp = new Date(date + "T00:00:00Z").getTime();
      var focusX = xForTime(timestamp);
      focusLine.setAttribute("x1", focusX);
      focusLine.setAttribute("x2", focusX);
      focusLine.setAttribute("visibility", "visible");
      var lines = [];
      ["high", "low"].forEach(function (group) {
        var row = displayed[group].find(function (candidate) {
          return candidate.date === date;
        });
        if (!row) return;
        lines.push(
          group.charAt(0).toUpperCase() + group.slice(1) + ": " +
          formatPercent(row.mean) + " (" + formatPercent(row.lower) + " to " +
          formatPercent(row.upper) + ")"
        );
        lines.push(
          "  " + row.contributing_jurisdictions + " of " + row.jurisdictions +
          " selected locations contributed"
        );
      });
      addTooltipLines(tooltip, date, lines);
      var shellRect = svg.parentElement.getBoundingClientRect();
      tooltip.style.left = Math.min(
        shellRect.width - 295,
        Math.max(8, clientX - shellRect.left + 12)
      ) + "px";
      tooltip.style.top = Math.max(8, clientY - shellRect.top - 30) + "px";
      tooltip.hidden = false;
    }

    overlay.addEventListener("pointermove", function (event) {
      var rect = svg.getBoundingClientRect();
      var svgX = (event.clientX - rect.left) / rect.width * width;
      var targetTime = minDate + (svgX - margin.left) / plotWidth * (maxDate - minDate);
      var nearest = uniqueDates[0];
      var distance = Infinity;
      uniqueDates.forEach(function (date) {
        var candidate = Math.abs(
          new Date(date + "T00:00:00Z").getTime() - targetTime
        );
        if (candidate < distance) {
          distance = candidate;
          nearest = date;
        }
      });
      showAt(nearest, event.clientX, event.clientY);
    });
    overlay.addEventListener("pointerleave", function () {
      tooltip.hidden = true;
      focusLine.setAttribute("visibility", "hidden");
    });
    overlay.addEventListener("focus", function () {
      var rect = svg.getBoundingClientRect();
      showAt(
        uniqueDates[Math.floor(uniqueDates.length / 2)],
        rect.left + rect.width / 2,
        rect.top + 80
      );
    });
    overlay.addEventListener("blur", function () {
      tooltip.hidden = true;
      focusLine.setAttribute("visibility", "hidden");
    });
  }

  function renderMetadata(container, counts, methodLabel, coverageLabel) {
    clear(container);
    [
      { label: "Low group", value: counts.low + " available", group: "low" },
      { label: "Excluded", value: counts.excluded + " available" },
      { label: "High group", value: counts.high + " available", group: "high" },
      { label: "Automatic coverage", value: coverageLabel },
      { label: "Aggregation", value: methodLabel }
    ].forEach(function (card) {
      var wrapper = document.createElement("div");
      wrapper.className = "vax-meta-card";
      if (card.group) wrapper.dataset.group = card.group;
      var label = document.createElement("span");
      label.className = "vax-meta-label";
      label.textContent = card.label;
      var value = document.createElement("span");
      value.className = "vax-meta-value";
      value.textContent = card.value;
      wrapper.appendChild(label);
      wrapper.appendChild(value);
      container.appendChild(wrapper);
    });
  }

  function initializeVaccinationExplorer(app) {
    var figureSelect = query(app, '[data-control="figure"]');
    var regionSelect = query(app, '[data-control="region"]');
    var ageSelect = query(app, '[data-control="age"]');
    var windowSelect = query(app, '[data-control="window"]');
    var lowInput = query(app, '[data-control="low-threshold"]');
    var highInput = query(app, '[data-control="high-threshold"]');
    var applyButton = query(app, '[data-action="apply-thresholds"]');
    var presetButton = query(app, '[data-action="manuscript-preset"]');
    var curvesButton = query(app, '[data-action="download-curves"]');
    var membershipButton = query(app, '[data-action="download-membership"]');
    var status = query(app, '[data-role="status"]');
    var metadata = query(app, '[data-role="metadata"]');
    var membershipBody = query(app, '[data-role="membership-body"]');
    var cache = new Map();
    var index = null;
    var shard = null;
    var assignmentsByContext = {};
    var assignmentMode = {};
    var groupedRows = { high: [], low: [] };

    function regionalMembership() {
      return index.membership.filter(function (row) {
        return row.region === regionSelect.value;
      });
    }

    function currentContextKey() {
      return panelKey(figureSelect.value, regionSelect.value, ageSelect.value);
    }

    function currentAssignments() {
      return assignmentsByContext[currentContextKey()] || {};
    }

    function availableSet() {
      return new Set((shard ? shard.series : []).map(function (series) {
        return series.geography;
      }));
    }

    function seriesByGeography() {
      var output = {};
      (shard ? shard.series : []).forEach(function (series) {
        output[series.geography] = series;
      });
      return output;
    }

    function configuredThresholds() {
      return index.thresholds[regionSelect.value];
    }

    function resetThresholdInputs() {
      var thresholds = configuredThresholds();
      lowInput.value = thresholds.low_below;
      highInput.value = thresholds.high_above;
      lowInput.setAttribute("aria-invalid", "false");
      highInput.setAttribute("aria-invalid", "false");
    }

    function readThresholds() {
      var low = Number(lowInput.value);
      var high = Number(highInput.value);
      var valid = Number.isFinite(low) && Number.isFinite(high) &&
        low >= 0 && high <= 100 && low < high;
      lowInput.setAttribute("aria-invalid", valid ? "false" : "true");
      highInput.setAttribute("aria-invalid", valid ? "false" : "true");
      return valid ? { low: low, high: high } : null;
    }

    function applyThresholds(renderAfter) {
      var thresholds = readThresholds();
      if (!thresholds) {
        status.textContent = "Low threshold must be smaller than the high threshold, within 0–100%.";
        return false;
      }
      if (!shard) return false;
      var panelSeries = seriesByGeography();
      var assignments = {};
      regionalMembership().forEach(function (row) {
        var automaticGroup = thresholdGroup(
          row.people_vaccinated_per_hundred,
          thresholds.low,
          thresholds.high
        );
        var series = panelSeries[row.geography];
        assignments[row.geography] = series && series.default_eligible ?
          automaticGroup : "excluded";
      });
      assignmentsByContext[currentContextKey()] = assignments;
      assignmentMode[currentContextKey()] =
        "vaccination thresholds + coverage rule";
      if (renderAfter && shard) renderDashboard();
      return true;
    }

    function loadManuscriptPreset() {
      resetThresholdInputs();
      var assignments = {};
      regionalMembership().forEach(function (row) {
        assignments[row.geography] = "excluded";
      });
      index.manuscript_presets.filter(function (row) {
        return row.figure === figureSelect.value && row.region === regionSelect.value;
      }).forEach(function (row) {
        assignments[row.geography] = row.vaccination_group;
      });
      assignmentsByContext[currentContextKey()] = assignments;
      assignmentMode[currentContextKey()] = "manuscript preset";
      renderDashboard();
    }

    function populateAges(preferred) {
      var ages = index.panels.filter(function (panel) {
        return panel.figure === figureSelect.value && panel.region === regionSelect.value;
      }).map(function (panel) {
        return panel.age_group;
      }).sort(function (left, right) {
        return AGE_ORDER.indexOf(left) - AGE_ORDER.indexOf(right);
      });
      var defaultAge = preferred;
      if (!defaultAge) {
        defaultAge = figureSelect.value === "figure_05" && regionSelect.value === "us" ?
          "65-84" : "60-79";
      }
      setOptions(ageSelect, ages.map(function (age) {
        return { value: age, label: age };
      }), defaultAge);
    }

    function currentPanel() {
      return index.panels.find(function (panel) {
        return panel.figure === figureSelect.value &&
          panel.region === regionSelect.value && panel.age_group === ageSelect.value;
      });
    }

    function renderMembershipTable() {
      clear(membershipBody);
      var available = availableSet();
      var panelSeries = seriesByGeography();
      var assignments = currentAssignments();
      var rows = regionalMembership().slice().sort(function (left, right) {
        return Number(right.people_vaccinated_per_hundred) -
          Number(left.people_vaccinated_per_hundred);
      });
      rows.forEach(function (row) {
        var isAvailable = available.has(row.geography);
        var seriesRecord = panelSeries[row.geography];
        var unavailableRecord = index.unavailable.find(function (candidate) {
          return candidate.figure === figureSelect.value &&
            candidate.region === regionSelect.value &&
            candidate.age_group === ageSelect.value &&
            candidate.geography === row.geography;
        });
        var tr = document.createElement("tr");
        if (seriesRecord && !seriesRecord.default_eligible) {
          tr.className = "vax-limited-coverage";
        }
        var location = document.createElement("td");
        location.textContent = row.geography_label;
        var rate = document.createElement("td");
        rate.textContent = formatRate(row.people_vaccinated_per_hundred);
        var date = document.createElement("td");
        date.textContent = row.measurement_date;
        var modelStatus = document.createElement("td");
        modelStatus.textContent = isAvailable ? "Available" : "Unavailable";
        modelStatus.className = isAvailable ?
          "vax-model-available" : "vax-model-unavailable";
        if (unavailableRecord && unavailableRecord.reason) {
          modelStatus.title = unavailableRecord.reason;
        } else if (seriesRecord && !seriesRecord.default_eligible) {
          modelStatus.title =
            "Available for manual use; excluded from automatic groups because " +
            "usable coverage is below the default threshold.";
        }
        var usable = document.createElement("td");
        usable.textContent = seriesRecord ?
          seriesRecord.usable_observations + " / " +
            seriesRecord.expected_observations : "Not available";
        var missing = document.createElement("td");
        missing.textContent = seriesRecord ?
          String(seriesRecord.missing_observations) : "Not available";
        var coverage = document.createElement("td");
        coverage.textContent = seriesRecord && finite(seriesRecord.coverage_fraction) ?
          (Number(seriesRecord.coverage_fraction) * 100).toFixed(1) :
          "Not available";
        if (seriesRecord && !seriesRecord.default_eligible) {
          coverage.className = "vax-coverage-limited";
          coverage.title = "Below the automatic 95% coverage threshold";
        }
        var groupCell = document.createElement("td");
        var select = document.createElement("select");
        select.setAttribute("aria-label", "Group for " + row.geography_label);
        [
          { value: "low", label: "Low" },
          { value: "excluded", label: "Excluded" },
          { value: "high", label: "High" }
        ].forEach(function (option) {
          var node = document.createElement("option");
          node.value = option.value;
          node.textContent = option.label;
          select.appendChild(node);
        });
        select.value = isAvailable ? (assignments[row.geography] || "excluded") : "excluded";
        select.disabled = !isAvailable;
        select.addEventListener("change", function () {
          currentAssignments()[row.geography] = select.value;
          assignmentMode[currentContextKey()] = "manual membership";
          renderDashboard();
        });
        groupCell.appendChild(select);
        tr.appendChild(location);
        tr.appendChild(rate);
        tr.appendChild(date);
        tr.appendChild(modelStatus);
        tr.appendChild(usable);
        tr.appendChild(missing);
        tr.appendChild(coverage);
        tr.appendChild(groupCell);
        membershipBody.appendChild(tr);
      });
    }

    function renderDashboard() {
      var assignments = currentAssignments();
      var available = availableSet();
      var counts = { high: 0, low: 0, excluded: 0 };
      regionalMembership().forEach(function (row) {
        if (!available.has(row.geography)) return;
        var group = assignments[row.geography] || "excluded";
        counts[group] += 1;
      });
      groupedRows = {
        high: aggregateFixedEffect(shard, assignments, "high"),
        low: aggregateFixedEffect(shard, assignments, "low")
      };
      if (figureSelect.value === "figure_05" && regionSelect.value === "europe") {
        groupedRows.high = smoothCenteredBox(groupedRows.high, 14);
        groupedRows.low = smoothCenteredBox(groupedRows.low, 14);
      }
      var methodLabel = "Inverse-variance fixed effect";
      if (figureSelect.value === "figure_05" && regionSelect.value === "europe") {
        methodLabel += " + 14-day smoothing";
      }
      var minimumCoveragePercent = Number(index.coverage.minimum_fraction) * 100;
      var coverageLabel = "≥" + minimumCoveragePercent.toFixed(0) +
        "% since " + index.coverage.start_date.slice(0, 4);
      var limitedCoverageCount = shard.series.filter(function (series) {
        return !series.default_eligible;
      }).length;
      renderMetadata(metadata, counts, methodLabel, coverageLabel);
      renderMembershipTable();
      renderChart(
        app,
        index,
        figureSelect.value,
        windowSelect.value,
        groupedRows
      );
      var unavailableCount = regionalMembership().length - available.size;
      status.textContent = counts.low + " low, " + counts.high + " high, " +
        counts.excluded + " excluded; " + limitedCoverageCount +
        " below the automatic coverage threshold; " + unavailableCount +
        " unavailable for this panel. Membership source: " +
        (assignmentMode[currentContextKey()] ||
          "vaccination thresholds + coverage rule") + ".";
      curvesButton.disabled = groupedRows.high.length + groupedRows.low.length === 0;
      membershipButton.disabled = false;
    }

    async function loadPanel() {
      var panel = currentPanel();
      if (!panel) throw new Error("No frozen panel matches this selection.");
      status.textContent = "Loading " + index.figures[panel.figure].label +
        " / " + regionSelect.value + " / " + ageSelect.value + " summaries…";
      var key = panelKey(panel.figure, panel.region, panel.age_group);
      if (!cache.has(key)) {
        cache.set(key, await fetchJSON(rootPath(app, panel.shard)));
      }
      shard = cache.get(key);
      if (!assignmentsByContext[currentContextKey()]) {
        applyThresholds(false);
      }
      renderDashboard();
    }

    function downloadCurves() {
      var thresholds = readThresholds() || { low: "", high: "" };
      var rows = groupedRows.high.concat(groupedRows.low).map(function (row) {
        return {
          figure: figureSelect.value,
          region: regionSelect.value,
          age_group: ageSelect.value,
          vaccination_group: row.vaccination_group,
          date: row.date,
          mean: row.mean,
          variance: row.variance,
          lower: row.lower,
          upper: row.upper,
          jurisdictions: row.jurisdictions,
          contributing_jurisdictions: row.contributing_jurisdictions,
          interval_method: row.interval_method,
          membership_source: assignmentMode[currentContextKey()],
          low_threshold: thresholds.low,
          high_threshold: thresholds.high
        };
      });
      downloadCSV(
        figureSelect.value + "_" + regionSelect.value + "_" +
          ageSelect.value.replace(/[^A-Za-z0-9]+/g, "-") + "_aggregated.csv",
        rows,
        [
          "figure", "region", "age_group", "vaccination_group", "date",
          "mean", "variance", "lower", "upper", "jurisdictions",
          "contributing_jurisdictions", "interval_method", "membership_source",
          "low_threshold", "high_threshold"
        ]
      );
    }

    function downloadMembership() {
      var available = availableSet();
      var panelSeries = seriesByGeography();
      var assignments = currentAssignments();
      var thresholds = readThresholds() || { low: "", high: "" };
      var rows = regionalMembership().map(function (row) {
        var isAvailable = available.has(row.geography);
        var seriesRecord = panelSeries[row.geography];
        return {
          figure: figureSelect.value,
          region: regionSelect.value,
          age_group: ageSelect.value,
          geography: row.geography,
          geography_label: row.geography_label,
          measurement_date: row.measurement_date,
          people_vaccinated_per_hundred: row.people_vaccinated_per_hundred,
          model_available: isAvailable,
          usable_observations: seriesRecord ?
            seriesRecord.usable_observations : "",
          expected_observations: seriesRecord ?
            seriesRecord.expected_observations : "",
          missing_observations: seriesRecord ?
            seriesRecord.missing_observations : "",
          coverage_fraction: seriesRecord ?
            seriesRecord.coverage_fraction : "",
          default_eligible: seriesRecord ?
            seriesRecord.default_eligible : false,
          selected_group: isAvailable ? assignments[row.geography] : "excluded",
          membership_source: assignmentMode[currentContextKey()],
          low_threshold: thresholds.low,
          high_threshold: thresholds.high
        };
      });
      downloadCSV(
        figureSelect.value + "_" + regionSelect.value + "_" +
          ageSelect.value.replace(/[^A-Za-z0-9]+/g, "-") + "_membership.csv",
        rows,
        [
          "figure", "region", "age_group", "geography", "geography_label",
          "measurement_date", "people_vaccinated_per_hundred", "model_available",
          "usable_observations", "expected_observations", "missing_observations",
          "coverage_fraction", "default_eligible",
          "selected_group", "membership_source", "low_threshold", "high_threshold"
        ]
      );
    }

    fetchJSON(rootPath(app, "index.json")).then(function (loadedIndex) {
      index = loadedIndex;
      resetThresholdInputs();
      populateAges("60-79");
      return loadPanel();
    }).catch(function (error) {
      renderNoData(query(app, '[data-role="chart"]'), "Unable to load vaccination summaries.");
      status.textContent = error.message;
      applyButton.disabled = true;
      presetButton.disabled = true;
      curvesButton.disabled = true;
      membershipButton.disabled = true;
    });

    figureSelect.addEventListener("change", function () {
      populateAges();
      resetThresholdInputs();
      loadPanel().catch(function (error) { status.textContent = error.message; });
    });
    regionSelect.addEventListener("change", function () {
      populateAges();
      resetThresholdInputs();
      loadPanel().catch(function (error) { status.textContent = error.message; });
    });
    ageSelect.addEventListener("change", function () {
      loadPanel().catch(function (error) { status.textContent = error.message; });
    });
    windowSelect.addEventListener("change", function () {
      if (shard) {
        renderChart(
          app,
          index,
          figureSelect.value,
          windowSelect.value,
          groupedRows
        );
      }
    });
    applyButton.addEventListener("click", function () { applyThresholds(true); });
    lowInput.addEventListener("change", function () { applyThresholds(true); });
    highInput.addEventListener("change", function () { applyThresholds(true); });
    presetButton.addEventListener("click", loadManuscriptPreset);
    curvesButton.addEventListener("click", downloadCurves);
    membershipButton.addEventListener("click", downloadMembership);
  }

  document.querySelectorAll("[data-vaccination-app]").forEach(function (app) {
    initializeVaccinationExplorer(app);
  });
}());

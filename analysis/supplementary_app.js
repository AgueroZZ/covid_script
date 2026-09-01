(function () {
  "use strict";

  var SVG_NS = "http://www.w3.org/2000/svg";
  var FAMILY_LABELS = {
    europe: "Eurostat Europe",
    us_non_sex: "United States — non-sex-stratified",
    us_sex: "United States — sex-stratified",
    canada_non_sex: "Canada — non-sex-stratified",
    canada_sex: "Canada — sex-stratified",
    england_wales: "England and Wales",
    ireland: "Republic of Ireland"
  };
  var MAP_FAMILIES = [
    "europe",
    "us_non_sex",
    "us_sex",
    "canada_non_sex",
    "canada_sex"
  ];
  var AGE_ORDER = [
    "0-44",
    "20-39",
    "25-44",
    "40-59",
    "45-64",
    "60-79",
    "65-84",
    "Under 65",
    "GE80",
    "GE85",
    "85+"
  ];
  var SEX_ORDER = ["total", "female", "male"];
  var numberFormatter = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 1
  });

  function query(root, selector) {
    return root.querySelector(selector);
  }

  function svgElement(name, attributes) {
    var element = document.createElementNS(SVG_NS, name);
    Object.keys(attributes || {}).forEach(function (key) {
      element.setAttribute(key, attributes[key]);
    });
    return element;
  }

  function clear(element) {
    while (element.firstChild) element.removeChild(element.firstChild);
  }

  function finite(value) {
    return value !== null && value !== "" && Number.isFinite(Number(value));
  }

  function numeric(value) {
    return finite(value) ? Number(value) : null;
  }

  function unique(values) {
    return Array.from(new Set(values));
  }

  function sortedAges(values) {
    return values.slice().sort(function (left, right) {
      var leftIndex = AGE_ORDER.indexOf(left);
      var rightIndex = AGE_ORDER.indexOf(right);
      if (leftIndex === -1) leftIndex = AGE_ORDER.length;
      if (rightIndex === -1) rightIndex = AGE_ORDER.length;
      return leftIndex - rightIndex || left.localeCompare(right);
    });
  }

  function sortedSexes(values) {
    return values.slice().sort(function (left, right) {
      return SEX_ORDER.indexOf(left) - SEX_ORDER.indexOf(right);
    });
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

  async function fetchJSON(path) {
    var response = await fetch(path, { cache: "no-cache" });
    if (!response.ok) {
      throw new Error("Unable to load " + path + " (HTTP " + response.status + ").");
    }
    return response.json();
  }

  function rootPath(app, relative) {
    return app.dataset.root.replace(/\/$/, "") + "/" + relative;
  }

  function renderMetadata(container, cards) {
    clear(container);
    cards.forEach(function (card) {
      var wrapper = document.createElement("div");
      wrapper.className = "supp-meta-card";
      var label = document.createElement("span");
      label.className = "supp-meta-label";
      label.textContent = card.label;
      var value = document.createElement("span");
      value.className = "supp-meta-value";
      value.textContent = card.value;
      wrapper.appendChild(label);
      wrapper.appendChild(value);
      container.appendChild(wrapper);
    });
  }

  function renderNoData(svg, message) {
    clear(svg);
    var text = svgElement("text", {
      x: 460,
      y: 240,
      class: "supp-no-data"
    });
    text.textContent = message;
    svg.appendChild(text);
  }

  function formatCount(value) {
    return finite(value) ? numberFormatter.format(Number(value)) : "Not available";
  }

  function formatPercent(value) {
    if (!finite(value)) return "Not available";
    var percent = Number(value) * 100;
    var digits = Math.abs(percent) < 10 ? 1 : 0;
    return percent.toFixed(digits) + "%";
  }

  function formatMetric(value, metric) {
    return metric === "pscore" ? formatPercent(value) : formatCount(value);
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

  function linePath(values, xScale, yScale) {
    var path = "";
    var active = false;
    values.forEach(function (value, index) {
      if (!finite(value)) {
        active = false;
        return;
      }
      path += (active ? "L" : "M") + xScale(index).toFixed(2) + "," +
        yScale(Number(value)).toFixed(2);
      active = true;
    });
    return path;
  }

  function areaPaths(lower, upper, xScale, yScale) {
    var segments = [];
    var current = [];
    lower.forEach(function (value, index) {
      if (finite(value) && finite(upper[index])) {
        current.push(index);
      } else if (current.length > 0) {
        segments.push(current);
        current = [];
      }
    });
    if (current.length > 0) segments.push(current);
    return segments.map(function (segment) {
      var forward = segment.map(function (index, position) {
        return (position === 0 ? "M" : "L") + xScale(index).toFixed(2) + "," +
          yScale(Number(upper[index])).toFixed(2);
      }).join("");
      var reverse = segment.slice().reverse().map(function (index) {
        return "L" + xScale(index).toFixed(2) + "," +
          yScale(Number(lower[index])).toFixed(2);
      }).join("");
      return forward + reverse + "Z";
    });
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

  function renderTimeChart(app, series, metric) {
    var svg = query(app, '[data-role="chart"]');
    var tooltip = query(app, '[data-role="tooltip"]');
    clear(svg);
    tooltip.hidden = true;

    var dates = series.date.map(function (value) {
      return new Date(value + "T00:00:00Z");
    });
    if (dates.length === 0) {
      renderNoData(svg, "No pointwise observations are available.");
      return;
    }
    var observed = series.observed;
    var center = metric === "mortality" ? series.expected_median : series.p_median;
    var lower = metric === "mortality" ? series.expected_lower : series.p_lower;
    var upper = metric === "mortality" ? series.expected_upper : series.p_upper;
    var candidates = center.concat(lower, upper);
    if (metric === "mortality") candidates = candidates.concat(observed);
    candidates = candidates.filter(finite).map(Number);
    if (candidates.length === 0) {
      renderNoData(svg, "The selected summary contains no finite values.");
      return;
    }

    var width = 920;
    var height = 480;
    var margin = { top: 40, right: 25, bottom: 58, left: 78 };
    var plotWidth = width - margin.left - margin.right;
    var plotHeight = height - margin.top - margin.bottom;
    var minDate = dates[0].getTime();
    var maxDate = dates[dates.length - 1].getTime();
    var yMin = Math.min.apply(null, candidates.concat([0]));
    var yMax = Math.max.apply(null, candidates.concat([0]));
    if (metric === "mortality") yMin = 0;
    if (yMin === yMax) {
      yMin -= 1;
      yMax += 1;
    }
    var padding = (yMax - yMin) * 0.05;
    if (metric !== "mortality") yMin -= padding;
    yMax += padding;

    function xForIndex(index) {
      return margin.left + (dates[index].getTime() - minDate) /
        Math.max(1, maxDate - minDate) * plotWidth;
    }
    function yFor(value) {
      return margin.top + (yMax - value) / (yMax - yMin) * plotHeight;
    }

    var yTicks = 5;
    for (var tick = 0; tick <= yTicks; tick += 1) {
      var yValue = yMin + (yMax - yMin) * tick / yTicks;
      var y = yFor(yValue);
      svg.appendChild(svgElement("line", {
        x1: margin.left,
        y1: y,
        x2: width - margin.right,
        y2: y,
        class: "supp-grid-line"
      }));
      var yLabel = svgElement("text", {
        x: margin.left - 10,
        y: y + 4,
        "text-anchor": "end",
        class: "supp-grid-label"
      });
      yLabel.textContent = formatMetric(yValue, metric);
      svg.appendChild(yLabel);
    }

    var xTicks = 5;
    for (var xTick = 0; xTick <= xTicks; xTick += 1) {
      var timestamp = minDate + (maxDate - minDate) * xTick / xTicks;
      var x = margin.left + plotWidth * xTick / xTicks;
      svg.appendChild(svgElement("line", {
        x1: x,
        y1: height - margin.bottom,
        x2: x,
        y2: height - margin.bottom + 6,
        class: "supp-axis"
      }));
      var xLabel = svgElement("text", {
        x: x,
        y: height - margin.bottom + 24,
        "text-anchor": "middle",
        class: "supp-grid-label"
      });
      xLabel.textContent = String(new Date(timestamp).getUTCFullYear());
      svg.appendChild(xLabel);
    }

    svg.appendChild(svgElement("line", {
      x1: margin.left,
      y1: margin.top,
      x2: margin.left,
      y2: height - margin.bottom,
      class: "supp-axis"
    }));
    svg.appendChild(svgElement("line", {
      x1: margin.left,
      y1: height - margin.bottom,
      x2: width - margin.right,
      y2: height - margin.bottom,
      class: "supp-axis"
    }));

    areaPaths(lower, upper, xForIndex, yFor).forEach(function (path) {
      svg.appendChild(svgElement("path", { d: path, class: "supp-interval" }));
    });
    svg.appendChild(svgElement("path", {
      d: linePath(center, xForIndex, yFor),
      class: "supp-expected-line"
    }));

    if (metric === "mortality") {
      svg.appendChild(svgElement("path", {
        d: linePath(observed, xForIndex, yFor),
        class: "supp-observed-line"
      }));
      var pointStep = Math.max(1, Math.ceil(observed.length / 260));
      observed.forEach(function (value, index) {
        if (index % pointStep === 0 && finite(value)) {
          svg.appendChild(svgElement("circle", {
            cx: xForIndex(index),
            cy: yFor(Number(value)),
            r: 1.8,
            class: "supp-observed-point"
          }));
        }
      });
    }

    var referenceDate = new Date("2020-01-01T00:00:00Z").getTime();
    if (referenceDate >= minDate && referenceDate <= maxDate) {
      var referenceX = margin.left + (referenceDate - minDate) /
        (maxDate - minDate) * plotWidth;
      svg.appendChild(svgElement("line", {
        x1: referenceX,
        y1: margin.top,
        x2: referenceX,
        y2: height - margin.bottom,
        class: "supp-reference-line"
      }));
    }
    if (metric === "pscore" && yMin <= 0 && yMax >= 0) {
      svg.appendChild(svgElement("line", {
        x1: margin.left,
        y1: yFor(0),
        x2: width - margin.right,
        y2: yFor(0),
        class: "supp-reference-line"
      }));
    }

    var yAxisLabel = svgElement("text", {
      x: 18,
      y: margin.top + plotHeight / 2,
      transform: "rotate(-90 18 " + (margin.top + plotHeight / 2) + ")",
      "text-anchor": "middle",
      class: "supp-chart-label"
    });
    yAxisLabel.textContent = metric === "mortality" ? "Deaths" : "P-score";
    svg.appendChild(yAxisLabel);

    var legend = svgElement("g", { transform: "translate(" + margin.left + " 18)" });
    var legendItems = metric === "mortality" ? [
      ["Observed", "#243b53"],
      ["Expected posterior median", "#2166ac"],
      ["95% posterior predictive interval", "#9fbfe0"]
    ] : [
      ["Posterior median P-score", "#2166ac"],
      ["95% posterior interval", "#9fbfe0"]
    ];
    var legendX = 0;
    legendItems.forEach(function (item) {
      legend.appendChild(svgElement("line", {
        x1: legendX,
        y1: 0,
        x2: legendX + 18,
        y2: 0,
        stroke: item[1],
        "stroke-width": item[0].indexOf("interval") === -1 ? 3 : 9,
        "stroke-opacity": item[0].indexOf("interval") === -1 ? 1 : 0.45
      }));
      var label = svgElement("text", {
        x: legendX + 24,
        y: 4,
        class: "supp-grid-label"
      });
      label.textContent = item[0];
      legend.appendChild(label);
      legendX += item[0].length * 6.3 + 48;
    });
    svg.appendChild(legend);

    var focusLine = svgElement("line", {
      x1: margin.left,
      y1: margin.top,
      x2: margin.left,
      y2: height - margin.bottom,
      class: "supp-focus-line",
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
      "aria-label": "Move across the plot to inspect dates"
    });
    svg.appendChild(overlay);

    function showAt(index, clientX, clientY) {
      focusLine.setAttribute("x1", xForIndex(index));
      focusLine.setAttribute("x2", xForIndex(index));
      focusLine.setAttribute("visibility", "visible");
      var lines;
      if (metric === "mortality") {
        lines = [
          "Observed: " + formatCount(observed[index]),
          "Expected median: " + formatCount(center[index]),
          "95% interval: " + formatCount(lower[index]) + " to " +
            formatCount(upper[index]),
          "Expected mean: " + formatCount(series.expected_mean[index])
        ];
      } else {
        lines = [
          "P-score median: " + formatPercent(center[index]),
          "95% interval: " + formatPercent(lower[index]) + " to " +
            formatPercent(upper[index])
        ];
      }
      addTooltipLines(tooltip, series.date[index], lines);
      var shellRect = svg.parentElement.getBoundingClientRect();
      tooltip.style.left = Math.min(
        shellRect.width - 270,
        Math.max(8, clientX - shellRect.left + 12)
      ) + "px";
      tooltip.style.top = Math.max(8, clientY - shellRect.top - 30) + "px";
      tooltip.hidden = false;
    }

    overlay.addEventListener("pointermove", function (event) {
      var rect = svg.getBoundingClientRect();
      var svgX = (event.clientX - rect.left) / rect.width * width;
      var targetTime = minDate + (svgX - margin.left) / plotWidth *
        (maxDate - minDate);
      var nearest = 0;
      var distance = Infinity;
      dates.forEach(function (date, index) {
        var candidate = Math.abs(date.getTime() - targetTime);
        if (candidate < distance) {
          distance = candidate;
          nearest = index;
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
      var middle = Math.floor(dates.length / 2);
      showAt(middle, rect.left + rect.width / 2, rect.top + 80);
    });
    overlay.addEventListener("blur", function () {
      tooltip.hidden = true;
      focusLine.setAttribute("visibility", "hidden");
    });
  }

  async function initializeTimeSeries(app) {
    var status = query(app, '[data-role="status"]');
    var metadata = query(app, '[data-role="metadata"]');
    var familySelect = query(app, '[data-control="family"]');
    var geographySelect = query(app, '[data-control="geography"]');
    var ageSelect = query(app, '[data-control="age"]');
    var sexSelect = query(app, '[data-control="sex"]');
    var metricSelect = query(app, '[data-control="metric"]');
    var downloadButton = query(app, '[data-action="download"]');
    var cache = new Map();
    var currentSeries = null;

    try {
      var index = await fetchJSON(rootPath(app, "index.json"));
      var cohorts = index.cohorts;
      var families = unique(cohorts.map(function (row) {
        return row.analysis_family;
      }));
      setOptions(familySelect, families.map(function (family) {
        return { value: family, label: FAMILY_LABELS[family] || family };
      }), "europe");

      function familyCohorts() {
        return cohorts.filter(function (row) {
          return row.analysis_family === familySelect.value;
        });
      }

      function populateGeography(preferred) {
        var rows = familyCohorts();
        var byGeography = {};
        rows.forEach(function (row) {
          byGeography[row.geography] = row.geography_label;
        });
        var options = Object.keys(byGeography).map(function (value) {
          return { value: value, label: byGeography[value] };
        }).sort(function (left, right) {
          return left.label.localeCompare(right.label);
        });
        setOptions(geographySelect, options, preferred);
      }

      function geographyCohorts() {
        return familyCohorts().filter(function (row) {
          return row.geography === geographySelect.value;
        });
      }

      function populateAge(preferred) {
        var ages = sortedAges(unique(geographyCohorts().map(function (row) {
          return row.age_group;
        })));
        setOptions(ageSelect, ages.map(function (age) {
          return { value: age, label: age };
        }), preferred);
      }

      function populateSex(preferred) {
        var sexes = sortedSexes(unique(geographyCohorts().filter(function (row) {
          return row.age_group === ageSelect.value;
        }).map(function (row) {
          return row.sex;
        })));
        setOptions(sexSelect, sexes.map(function (sex) {
          return { value: sex, label: sex.charAt(0).toUpperCase() + sex.slice(1) };
        }), preferred);
      }

      function selectedCohort() {
        return cohorts.find(function (row) {
          return row.analysis_family === familySelect.value &&
            row.geography === geographySelect.value &&
            row.age_group === ageSelect.value && row.sex === sexSelect.value;
        });
      }

      async function renderSelection() {
        var cohort = selectedCohort();
        currentSeries = null;
        if (!cohort) {
          renderNoData(query(app, '[data-role="chart"]'), "No registered cohort matches this selection.");
          status.textContent = "No registered cohort matches this selection.";
          return;
        }
        renderMetadata(metadata, [
          { label: "Analysis family", value: FAMILY_LABELS[cohort.analysis_family] },
          { label: "Geography", value: cohort.geography_label },
          { label: "Stratification", value: cohort.age_group + " / " + cohort.sex },
          { label: "Source frequency", value: cohort.frequency },
          { label: "Registry status", value: cohort.status }
        ]);
        if (cohort.status !== "available") {
          renderNoData(
            query(app, '[data-role="chart"]'),
            "Model unavailable: " + (cohort.error_message || cohort.status)
          );
          status.textContent = "This registered cohort is unavailable; no value was imputed.";
          downloadButton.disabled = true;
          return;
        }
        status.textContent = "Loading " + cohort.geography_label + " summary…";
        if (!cache.has(cohort.shard)) {
          cache.set(cohort.shard, await fetchJSON(rootPath(app, cohort.shard)));
        }
        var shard = cache.get(cohort.shard);
        currentSeries = shard.series.find(function (series) {
          return series.analysis_id === cohort.analysis_id;
        });
        if (!currentSeries) {
          throw new Error("The selected cohort is absent from its geography shard.");
        }
        renderTimeChart(app, currentSeries, metricSelect.value);
        status.textContent = numberFormatter.format(currentSeries.date.length) +
          " observed periods loaded from a summary-only geography shard.";
        downloadButton.disabled = false;
      }

      populateGeography("NL");
      populateAge("GE80");
      populateSex("total");

      familySelect.addEventListener("change", async function () {
        populateGeography();
        populateAge();
        populateSex();
        await renderSelection();
      });
      geographySelect.addEventListener("change", async function () {
        populateAge();
        populateSex();
        await renderSelection();
      });
      ageSelect.addEventListener("change", async function () {
        populateSex();
        await renderSelection();
      });
      sexSelect.addEventListener("change", renderSelection);
      metricSelect.addEventListener("change", function () {
        if (currentSeries) renderTimeChart(app, currentSeries, metricSelect.value);
      });
      downloadButton.addEventListener("click", function () {
        if (!currentSeries) return;
        var rows = currentSeries.date.map(function (date, index) {
          return {
            analysis_id: currentSeries.analysis_id,
            date: date,
            observed_deaths: currentSeries.observed[index],
            expected_mean: currentSeries.expected_mean[index],
            expected_median: currentSeries.expected_median[index],
            expected_lower: currentSeries.expected_lower[index],
            expected_upper: currentSeries.expected_upper[index],
            p_mean: currentSeries.p_mean[index],
            p_lower: currentSeries.p_lower[index],
            p_median: currentSeries.p_median[index],
            p_upper: currentSeries.p_upper[index]
          };
        });
        downloadCSV(
          currentSeries.analysis_id + ".csv",
          rows,
          [
            "analysis_id", "date", "observed_deaths", "expected_mean",
            "expected_median", "expected_lower", "expected_upper", "p_mean",
            "p_lower", "p_median", "p_upper"
          ]
        );
      });
      await renderSelection();
    } catch (error) {
      renderNoData(query(app, '[data-role="chart"]'), "Unable to load the supplementary explorer.");
      status.textContent = error.message;
      downloadButton.disabled = true;
    }
  }

  function forEachCoordinate(coordinates, callback) {
    if (coordinates.length >= 2 && typeof coordinates[0] === "number") {
      callback(coordinates[0], coordinates[1]);
      return;
    }
    coordinates.forEach(function (child) {
      forEachCoordinate(child, callback);
    });
  }

  function geometryBounds(features) {
    var bounds = [Infinity, Infinity, -Infinity, -Infinity];
    features.forEach(function (feature) {
      forEachCoordinate(feature.geometry.coordinates, function (x, y) {
        bounds[0] = Math.min(bounds[0], x);
        bounds[1] = Math.min(bounds[1], y);
        bounds[2] = Math.max(bounds[2], x);
        bounds[3] = Math.max(bounds[3], y);
      });
    });
    return bounds;
  }

  function geometryPath(geometry, project) {
    function ringPath(ring) {
      return ring.map(function (point, index) {
        var projected = project(point[0], point[1]);
        return (index === 0 ? "M" : "L") + projected[0].toFixed(2) + "," +
          projected[1].toFixed(2);
      }).join("") + "Z";
    }
    if (geometry.type === "Polygon") {
      return geometry.coordinates.map(ringPath).join("");
    }
    if (geometry.type === "MultiPolygon") {
      return geometry.coordinates.map(function (polygon) {
        return polygon.map(ringPath).join("");
      }).join("");
    }
    return "";
  }

  function mixColor(left, right, proportion) {
    function channels(hex) {
      return [
        parseInt(hex.slice(1, 3), 16),
        parseInt(hex.slice(3, 5), 16),
        parseInt(hex.slice(5, 7), 16)
      ];
    }
    var a = channels(left);
    var b = channels(right);
    var mixed = a.map(function (value, index) {
      return Math.round(value + (b[index] - value) * proportion);
    });
    return "#" + mixed.map(function (value) {
      return value.toString(16).padStart(2, "0");
    }).join("");
  }

  function divergingColor(value, domain) {
    if (!finite(value)) return "#d9e2ec";
    var clipped = Math.max(-domain, Math.min(domain, Number(value)));
    if (clipped < 0) {
      return mixColor("#2166ac", "#f7f7f7", (clipped + domain) / domain);
    }
    return mixColor("#f7f7f7", "#b2182b", clipped / domain);
  }

  function mapStatisticLabel(statistic) {
    return {
      p_median: "Posterior median P-score",
      p_lower: "P-score 2.5th percentile",
      p_upper: "P-score 97.5th percentile",
      delta_median: "Posterior median excess deaths"
    }[statistic];
  }

  function renderLegend(container, statistic, domain) {
    clear(container);
    [-1, -0.5, 0, 0.5, 1].forEach(function (fraction) {
      var stop = document.createElement("span");
      stop.className = "supp-legend-stop";
      var color = document.createElement("span");
      color.className = "supp-legend-color";
      color.style.background = divergingColor(fraction * domain, domain);
      var label = document.createElement("span");
      label.textContent = statistic.indexOf("p_") === 0 ?
        formatPercent(fraction * domain) : formatCount(fraction * domain);
      stop.appendChild(color);
      stop.appendChild(label);
      container.appendChild(stop);
    });
  }

  function renderMapTable(container, rows, statistic) {
    clear(container);
    var table = document.createElement("table");
    var thead = document.createElement("thead");
    var header = document.createElement("tr");
    ["Geography", "Estimate", "95% interval", "Status"].forEach(function (text) {
      var th = document.createElement("th");
      th.textContent = text;
      header.appendChild(th);
    });
    thead.appendChild(header);
    table.appendChild(thead);
    var tbody = document.createElement("tbody");
    var metric = statistic.indexOf("p_") === 0 ? "pscore" : "mortality";
    rows.slice().sort(function (left, right) {
      var leftValue = numeric(left[statistic]);
      var rightValue = numeric(right[statistic]);
      if (leftValue === null && rightValue === null) {
        return left.geography_label.localeCompare(right.geography_label);
      }
      if (leftValue === null) return 1;
      if (rightValue === null) return -1;
      return rightValue - leftValue;
    }).forEach(function (row) {
      var tr = document.createElement("tr");
      var interval = statistic.indexOf("p_") === 0 ?
        formatPercent(row.p_lower) + " to " + formatPercent(row.p_upper) :
        formatCount(row.delta_lower) + " to " + formatCount(row.delta_upper);
      [
        row.geography_label,
        formatMetric(row[statistic], metric),
        interval,
        row.status
      ].forEach(function (text) {
        var td = document.createElement("td");
        td.textContent = text;
        tr.appendChild(td);
      });
      tbody.appendChild(tr);
    });
    table.appendChild(tbody);
    container.appendChild(table);
  }

  function renderMap(app, geometry, rows, family, statistic) {
    var svg = query(app, '[data-role="map"]');
    var tooltip = query(app, '[data-role="tooltip"]');
    clear(svg);
    tooltip.hidden = true;
    var familyMapIds = new Set(rows.map(function (row) { return row.map_id; }));
    var features = geometry.features.filter(function (feature) {
      return familyMapIds.has(feature.properties.map_id);
    });
    if (features.length === 0) {
      renderNoData(svg, "No map geometry is available for this selection.");
      return 0;
    }
    var width = 920;
    var height = 560;
    var padding = 34;
    var bounds = geometryBounds(features);
    var scale = Math.min(
      (width - 2 * padding) / (bounds[2] - bounds[0]),
      (height - 2 * padding) / (bounds[3] - bounds[1])
    );
    var drawnWidth = (bounds[2] - bounds[0]) * scale;
    var drawnHeight = (bounds[3] - bounds[1]) * scale;
    var xOffset = (width - drawnWidth) / 2;
    var yOffset = (height - drawnHeight) / 2;
    function project(x, y) {
      return [
        xOffset + (x - bounds[0]) * scale,
        height - yOffset - (y - bounds[1]) * scale
      ];
    }
    var byMapId = {};
    rows.forEach(function (row) { byMapId[row.map_id] = row; });
    var finiteValues = rows.map(function (row) {
      return numeric(row[statistic]);
    }).filter(function (value) { return value !== null; });
    var domain = statistic.indexOf("p_") === 0 ? 0.5 :
      Math.max(1, Math.max.apply(null, finiteValues.map(Math.abs)));

    features.forEach(function (feature) {
      var mapId = feature.properties.map_id;
      var row = byMapId[mapId];
      var value = row ? row[statistic] : null;
      var path = svgElement("path", {
        d: geometryPath(feature.geometry, project),
        fill: divergingColor(value, domain),
        class: "supp-map-path",
        tabindex: 0,
        "fill-rule": "evenodd"
      });
      var title = svgElement("title");
      title.textContent = row ?
        row.geography_label + ": " + formatMetric(
          value,
          statistic.indexOf("p_") === 0 ? "pscore" : "mortality"
        ) : mapId + ": no registered result";
      path.appendChild(title);
      path.setAttribute("aria-label", title.textContent);

      function show(event) {
        var lines;
        if (!row) {
          lines = ["No registered result for this stratification."];
        } else if (statistic.indexOf("p_") === 0) {
          lines = [
            mapStatisticLabel(statistic) + ": " + formatPercent(row[statistic]),
            "95% P-score interval: " + formatPercent(row.p_lower) + " to " +
              formatPercent(row.p_upper),
            "Status: " + row.status
          ];
        } else {
          lines = [
            "Median excess deaths: " + formatCount(row.delta_median),
            "95% interval: " + formatCount(row.delta_lower) + " to " +
              formatCount(row.delta_upper),
            "Status: " + row.status
          ];
        }
        addTooltipLines(tooltip, row ? row.geography_label : mapId, lines);
        var shell = svg.parentElement.getBoundingClientRect();
        var clientX = event.clientX || shell.left + 30;
        var clientY = event.clientY || shell.top + 50;
        tooltip.style.left = Math.min(
          shell.width - 270,
          Math.max(8, clientX - shell.left + 12)
        ) + "px";
        tooltip.style.top = Math.max(8, clientY - shell.top - 20) + "px";
        tooltip.hidden = false;
      }
      path.addEventListener("pointerenter", show);
      path.addEventListener("pointermove", show);
      path.addEventListener("pointerleave", function () { tooltip.hidden = true; });
      path.addEventListener("focus", show);
      path.addEventListener("blur", function () { tooltip.hidden = true; });
      svg.appendChild(path);
    });
    renderLegend(query(app, '[data-role="legend"]'), statistic, domain);
    return features.length;
  }

  async function initializeMap(app) {
    var status = query(app, '[data-role="status"]');
    var metadata = query(app, '[data-role="metadata"]');
    var familySelect = query(app, '[data-control="family"]');
    var ageSelect = query(app, '[data-control="age"]');
    var sexSelect = query(app, '[data-control="sex"]');
    var waveSelect = query(app, '[data-control="wave"]');
    var statisticSelect = query(app, '[data-control="statistic"]');
    var tableContainer = query(app, '[data-role="table"]');
    var geometryNote = query(app, '[data-role="geometry-note"]');
    var downloadButton = query(app, '[data-action="download"]');
    var geometryCache = new Map();
    var currentRows = [];

    try {
      var loaded = await Promise.all([
        fetchJSON(rootPath(app, "index.json")),
        fetchJSON(rootPath(app, "wave_summary.json"))
      ]);
      var index = loaded[0];
      var waves = loaded[1];
      var cohorts = index.cohorts;
      setOptions(familySelect, MAP_FAMILIES.map(function (family) {
        return { value: family, label: FAMILY_LABELS[family] };
      }), "europe");
      setOptions(waveSelect, index.waves.map(function (wave) {
        return {
          value: wave.wave,
          label: wave.wave.charAt(0).toUpperCase() + wave.wave.slice(1)
        };
      }), "initial");

      function familyCohorts() {
        return cohorts.filter(function (row) {
          return row.analysis_family === familySelect.value;
        });
      }

      function populateAge(preferred) {
        var ages = sortedAges(unique(familyCohorts().map(function (row) {
          return row.age_group;
        })));
        setOptions(ageSelect, ages.map(function (age) {
          return { value: age, label: age };
        }), preferred);
      }

      function populateSex(preferred) {
        var sexes = sortedSexes(unique(familyCohorts().filter(function (row) {
          return row.age_group === ageSelect.value;
        }).map(function (row) { return row.sex; })));
        setOptions(sexSelect, sexes.map(function (sex) {
          return { value: sex, label: sex.charAt(0).toUpperCase() + sex.slice(1) };
        }), preferred);
      }

      async function geometryForFamily() {
        var key = familySelect.value === "europe" ? "europe" : "north_america";
        if (!geometryCache.has(key)) {
          geometryCache.set(key, await fetchJSON(rootPath(app, index.geometry[key])));
        }
        return geometryCache.get(key);
      }

      async function renderSelection() {
        status.textContent = "Loading selected wave summary…";
        currentRows = waves.filter(function (row) {
          return row.analysis_family === familySelect.value &&
            row.age_group === ageSelect.value && row.sex === sexSelect.value &&
            row.wave === waveSelect.value;
        });
        var geometry = await geometryForFamily();
        var drawn = renderMap(
          app,
          geometry,
          currentRows,
          familySelect.value,
          statisticSelect.value
        );
        renderMapTable(tableContainer, currentRows, statisticSelect.value);
        var wave = index.waves.find(function (candidate) {
          return candidate.wave === waveSelect.value;
        });
        var successful = currentRows.filter(function (row) {
          return row.status === "success" && finite(row[statisticSelect.value]);
        }).length;
        renderMetadata(metadata, [
          { label: "Map collection", value: FAMILY_LABELS[familySelect.value] },
          { label: "Stratification", value: ageSelect.value + " / " + sexSelect.value },
          { label: "Wave", value: waveSelect.options[waveSelect.selectedIndex].text },
          { label: "Wave interval", value: wave.start + " to " + wave.end_exclusive },
          { label: "Finite estimates", value: successful + " of " + currentRows.length }
        ]);
        geometryNote.textContent = statisticSelect.value.indexOf("p_") === 0 ?
          "Fixed color domain: -50% to +50%; exact unclipped values appear in the table and tooltips." :
          "Selection-specific symmetric color domain; compare exact values rather than colors across selections.";
        if (familySelect.value.indexOf("us_") === 0) {
          geometryNote.textContent += " Alaska and Hawaii are retained in the table but not in the contiguous map geometry.";
        }
        status.textContent = drawn + " geographic shapes rendered; " +
          currentRows.length + " registered results listed.";
        downloadButton.disabled = currentRows.length === 0;
      }

      populateAge("40-59");
      populateSex("total");
      familySelect.addEventListener("change", async function () {
        populateAge();
        populateSex();
        await renderSelection();
      });
      ageSelect.addEventListener("change", async function () {
        populateSex();
        await renderSelection();
      });
      sexSelect.addEventListener("change", renderSelection);
      waveSelect.addEventListener("change", renderSelection);
      statisticSelect.addEventListener("change", renderSelection);
      downloadButton.addEventListener("click", function () {
        if (currentRows.length === 0) return;
        downloadCSV(
          [
            familySelect.value,
            ageSelect.value,
            sexSelect.value,
            waveSelect.value
          ].join("__") + ".csv",
          currentRows,
          [
            "analysis_id", "analysis_family", "geography", "geography_label",
            "map_id", "age_group", "sex", "frequency", "wave", "start",
            "end_exclusive", "observed_periods", "observed_deaths",
            "expected_lower", "expected_median", "expected_upper",
            "delta_lower", "delta_median", "delta_upper", "p_lower",
            "p_median", "p_upper", "posterior_draws", "status"
          ]
        );
      });
      await renderSelection();
    } catch (error) {
      renderNoData(query(app, '[data-role="map"]'), "Unable to load the wave-map explorer.");
      status.textContent = error.message;
      downloadButton.disabled = true;
    }
  }

  function boot() {
    document.querySelectorAll("[data-supplementary-app]").forEach(function (app) {
      if (app.dataset.supplementaryApp === "timeseries") {
        initializeTimeSeries(app);
      } else if (app.dataset.supplementaryApp === "map") {
        initializeMap(app);
      }
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
}());

(function () {
  "use strict";

  var WAVE_COLUMNS = ["initial", "alpha", "delta", "omicron"];

  function rootPath(app, path) {
    return app.dataset.root.replace(/\/$/, "") + "/" + path.replace(/^\//, "");
  }

  function fetchJSON(path) {
    return fetch(path, { cache: "no-store" }).then(function (response) {
      if (!response.ok) {
        throw new Error("Request failed with status " + response.status + ".");
      }
      return response.json();
    });
  }

  function isMissing(value) {
    return value === null || value === undefined || value === "";
  }

  function displayValue(row, column) {
    var value = row[column.key];
    if (isMissing(value)) return "NA";
    if (column.type === "number") {
      var numeric = Number(value);
      return Number.isFinite(numeric) ? numeric.toFixed(2) : "NA";
    }
    return String(value);
  }

  function searchableValue(row, column) {
    return displayValue(row, column).toLocaleLowerCase();
  }

  function searchTokens(value) {
    return String(value || "")
      .trim()
      .toLocaleLowerCase()
      .split(/\s+/)
      .filter(function (token) { return token.length > 0; });
  }

  function validateIndex(index) {
    if (!index || !Array.isArray(index.columns) || !Array.isArray(index.rows)) {
      throw new Error("The frozen table index is incomplete.");
    }
    var keys = index.columns.map(function (column) { return column.key; });
    if (new Set(keys).size !== keys.length) {
      throw new Error("The frozen table index has duplicated columns.");
    }
    var required = [
      "region_set",
      "geography",
      "people_vaccinated_per_hundred",
      "estimand_age_group",
      "initial",
      "alpha",
      "delta",
      "omicron",
      "result_status",
      "in_manuscript_table_1"
    ];
    required.forEach(function (key) {
      if (keys.indexOf(key) < 0) {
        throw new Error("The frozen table index is missing " + key + ".");
      }
    });
    if (Number(index.row_count) !== index.rows.length) {
      throw new Error("The frozen table row count does not match its metadata.");
    }
  }

  function compareRows(left, right, column, direction) {
    var leftValue = left[column.key];
    var rightValue = right[column.key];
    var leftMissing = isMissing(leftValue);
    var rightMissing = isMissing(rightValue);
    if (leftMissing && rightMissing) return 0;
    if (leftMissing) return 1;
    if (rightMissing) return -1;

    var comparison;
    if (column.type === "number") {
      comparison = Number(leftValue) - Number(rightValue);
    } else {
      comparison = String(leftValue).localeCompare(String(rightValue), undefined, {
        numeric: true,
        sensitivity: "base"
      });
    }
    return direction === "asc" ? comparison : -comparison;
  }

  function paginationItems(current, total) {
    var pages = new Set([1, total]);
    for (var page = current - 2; page <= current + 2; page += 1) {
      if (page >= 1 && page <= total) pages.add(page);
    }
    var sorted = Array.from(pages).sort(function (left, right) {
      return left - right;
    });
    var output = [];
    sorted.forEach(function (page, index) {
      if (index > 0 && page - sorted[index - 1] > 1) output.push("gap");
      output.push(page);
    });
    return output;
  }

  function csvCell(value) {
    var text = isMissing(value) ? "NA" : String(value);
    return '"' + text.replace(/"/g, '""') + '"';
  }

  function downloadCSV(columns, rows) {
    var header = columns.map(function (column) {
      return csvCell(column.key);
    }).join(",");
    var lines = rows.map(function (row) {
      return columns.map(function (column) {
        return csvCell(row[column.key]);
      }).join(",");
    });
    var blob = new Blob(["\ufeff", [header].concat(lines).join("\n") + "\n"], {
      type: "text/csv;charset=utf-8"
    });
    var url = URL.createObjectURL(blob);
    var link = document.createElement("a");
    link.href = url;
    link.download = "supplementary_table_explorer_filtered.csv";
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }

  function initializeTableExplorer(app) {
    var globalSearch = app.querySelector('[data-control="global-search"]');
    var pageSize = app.querySelector('[data-control="page-size"]');
    var manuscriptButton = app.querySelector('[data-action="manuscript"]');
    var clearButton = app.querySelector('[data-action="clear"]');
    var downloadButton = app.querySelector('[data-action="download"]');
    var metadata = app.querySelector('[data-role="metadata"]');
    var tableHead = app.querySelector('[data-role="table-head"]');
    var tableBody = app.querySelector('[data-role="table-body"]');
    var tableShell = app.querySelector(".tablex-table-shell");
    var status = app.querySelector('[data-role="status"]');
    var pagination = app.querySelector('[data-role="pagination"]');

    var state = {
      index: null,
      columns: [],
      rows: [],
      filteredRows: [],
      columnFilters: {},
      filterInputs: {},
      globalQuery: "",
      manuscriptOnly: false,
      sortKey: null,
      sortDirection: "asc",
      page: 1,
      pageSize: Number(pageSize.value) || 25
    };

    function columnByKey(key) {
      return state.columns.find(function (column) {
        return column.key === key;
      });
    }

    function updateManuscriptButton() {
      manuscriptButton.setAttribute(
        "aria-pressed",
        state.manuscriptOnly ? "true" : "false"
      );
      manuscriptButton.textContent = state.manuscriptOnly
        ? "Show all supplementary rows"
        : "Show manuscript Table 1 rows";
    }

    function updateSortIndicators() {
      state.columns.forEach(function (column) {
        var header = tableHead.querySelector(
          'th[data-column="' + column.key + '"]'
        );
        if (!header) return;
        var active = state.sortKey === column.key;
        header.setAttribute(
          "aria-sort",
          active ? (state.sortDirection === "asc" ? "ascending" : "descending") : "none"
        );
        var indicator = header.querySelector(".tablex-sort-indicator");
        indicator.textContent = active
          ? (state.sortDirection === "asc" ? "▲" : "▼")
          : "↕";
      });
    }

    function buildHeader() {
      var headingRow = document.createElement("tr");
      var filterRow = document.createElement("tr");
      filterRow.className = "tablex-filter-row";

      state.columns.forEach(function (column) {
        var heading = document.createElement("th");
        heading.scope = "col";
        heading.dataset.column = column.key;
        heading.setAttribute("aria-sort", "none");

        var sortButton = document.createElement("button");
        sortButton.type = "button";
        sortButton.className = "tablex-sort-button";
        sortButton.dataset.sort = column.key;
        sortButton.setAttribute("aria-label", "Sort by " + column.label);

        var label = document.createElement("span");
        label.textContent = column.label;
        var indicator = document.createElement("span");
        indicator.className = "tablex-sort-indicator";
        indicator.setAttribute("aria-hidden", "true");
        indicator.textContent = "↕";
        sortButton.appendChild(label);
        sortButton.appendChild(indicator);
        sortButton.addEventListener("click", function () {
          if (state.sortKey === column.key) {
            state.sortDirection = state.sortDirection === "asc" ? "desc" : "asc";
          } else {
            state.sortKey = column.key;
            state.sortDirection = "asc";
          }
          state.page = 1;
          renderData();
        });
        heading.appendChild(sortButton);
        headingRow.appendChild(heading);

        var filterCell = document.createElement("th");
        filterCell.scope = "col";
        var filter = document.createElement("input");
        filter.type = "search";
        filter.className = "tablex-column-filter";
        filter.placeholder = "Filter " + column.label;
        filter.setAttribute("aria-label", "Filter " + column.label);
        filter.autocomplete = "off";
        filter.addEventListener("input", function () {
          state.columnFilters[column.key] = filter.value;
          state.page = 1;
          renderData();
        });
        state.filterInputs[column.key] = filter;
        filterCell.appendChild(filter);
        filterRow.appendChild(filterCell);
      });

      tableHead.replaceChildren(headingRow, filterRow);
    }

    function filterRows() {
      var globalTokens = searchTokens(state.globalQuery);
      var activeColumnFilters = Object.keys(state.columnFilters).filter(function (key) {
        return searchTokens(state.columnFilters[key]).length > 0;
      });
      var filtered = state.rows.filter(function (row) {
        if (state.manuscriptOnly && row.in_manuscript_table_1 !== "Yes") {
          return false;
        }
        if (globalTokens.length > 0) {
          var combined = state.columns.map(function (column) {
            return searchableValue(row, column);
          }).join(" ");
          if (!globalTokens.every(function (token) {
            return combined.indexOf(token) >= 0;
          })) return false;
        }
        return activeColumnFilters.every(function (key) {
          var column = columnByKey(key);
          var tokens = searchTokens(state.columnFilters[key]);
          var value = searchableValue(row, column);
          return tokens.every(function (token) {
            return value.indexOf(token) >= 0;
          });
        });
      });

      if (state.sortKey) {
        var sortColumn = columnByKey(state.sortKey);
        filtered = filtered.map(function (row, index) {
          return { row: row, originalIndex: index };
        }).sort(function (left, right) {
          var comparison = compareRows(
            left.row,
            right.row,
            sortColumn,
            state.sortDirection
          );
          return comparison === 0
            ? left.originalIndex - right.originalIndex
            : comparison;
        }).map(function (item) { return item.row; });
      }
      return filtered;
    }

    function renderMetadata() {
      var regionCount = Array.isArray(state.index.regions)
        ? state.index.regions.filter(function (row) { return Number(row.rows) > 0; }).length
        : 0;
      var cards = [
        ["Frozen rows", state.rows.length.toLocaleString()],
        ["Matching rows", state.filteredRows.length.toLocaleString()],
        [
          "Manuscript match",
          String(state.index.manuscript_rows_exact) + "/" +
            String(state.index.manuscript_row_count)
        ],
        ["Regions", regionCount.toLocaleString()]
      ];
      metadata.replaceChildren();
      cards.forEach(function (card) {
        var container = document.createElement("div");
        container.className = "tablex-meta-card";
        var label = document.createElement("span");
        label.className = "tablex-meta-label";
        label.textContent = card[0];
        var value = document.createElement("span");
        value.className = "tablex-meta-value";
        value.textContent = card[1];
        container.appendChild(label);
        container.appendChild(value);
        metadata.appendChild(container);
      });
    }

    function renderBody(rows) {
      tableBody.replaceChildren();
      if (rows.length === 0) {
        var emptyRow = document.createElement("tr");
        emptyRow.className = "tablex-empty";
        var emptyCell = document.createElement("td");
        emptyCell.colSpan = state.columns.length;
        emptyCell.textContent = "No rows match the current filters.";
        emptyRow.appendChild(emptyCell);
        tableBody.appendChild(emptyRow);
        return;
      }

      var fragment = document.createDocumentFragment();
      rows.forEach(function (row) {
        var tableRow = document.createElement("tr");
        state.columns.forEach(function (column) {
          var cell = document.createElement("td");
          var value = displayValue(row, column);
          cell.textContent = value;
          if (value === "NA") cell.classList.add("tablex-na");
          if (column.type === "number") cell.classList.add("tablex-number");
          if (column.key === "vaccination_measurement_date") {
            cell.classList.add("tablex-date");
          }
          if (WAVE_COLUMNS.indexOf(column.key) >= 0) {
            cell.classList.add("tablex-wave");
            if (value.indexOf("unavailable") >= 0) {
              cell.classList.add("tablex-na");
            }
          }
          if (column.key === "result_status") {
            cell.dataset.status = String(row.result_status || "unavailable");
          }
          tableRow.appendChild(cell);
        });
        fragment.appendChild(tableRow);
      });
      tableBody.appendChild(fragment);
    }

    function createPageButton(label, target, disabled, current, ariaLabel) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = "tablex-page-button";
      button.textContent = label;
      button.disabled = disabled;
      if (current) button.setAttribute("aria-current", "page");
      if (ariaLabel) button.setAttribute("aria-label", ariaLabel);
      button.addEventListener("click", function () {
        state.page = target;
        tableShell.scrollTop = 0;
        renderData();
      });
      return button;
    }

    function renderPagination(pageCount) {
      pagination.replaceChildren();
      pagination.appendChild(createPageButton(
        "First", 1, state.page === 1, false, "First page"
      ));
      pagination.appendChild(createPageButton(
        "Previous",
        Math.max(1, state.page - 1),
        state.page === 1,
        false,
        "Previous page"
      ));

      paginationItems(state.page, pageCount).forEach(function (item) {
        if (item === "gap") {
          var gap = document.createElement("span");
          gap.className = "tablex-page-gap";
          gap.textContent = "…";
          gap.setAttribute("aria-hidden", "true");
          pagination.appendChild(gap);
        } else {
          pagination.appendChild(createPageButton(
            String(item),
            item,
            item === state.page,
            item === state.page,
            "Page " + item
          ));
        }
      });

      pagination.appendChild(createPageButton(
        "Next",
        Math.min(pageCount, state.page + 1),
        state.page === pageCount,
        false,
        "Next page"
      ));
      pagination.appendChild(createPageButton(
        "Last", pageCount, state.page === pageCount, false, "Last page"
      ));
    }

    function renderData() {
      state.filteredRows = filterRows();
      var pageCount = Math.max(
        1,
        Math.ceil(state.filteredRows.length / state.pageSize)
      );
      state.page = Math.min(Math.max(1, state.page), pageCount);
      var start = (state.page - 1) * state.pageSize;
      var end = Math.min(start + state.pageSize, state.filteredRows.length);
      renderBody(state.filteredRows.slice(start, end));
      renderPagination(pageCount);
      renderMetadata();
      updateSortIndicators();
      updateManuscriptButton();

      status.textContent = state.filteredRows.length === 0
        ? "Showing 0 of 0 matching rows (" + state.rows.length + " frozen rows)."
        : "Showing " + (start + 1) + "–" + end + " of " +
          state.filteredRows.length + " matching rows (" +
          state.rows.length + " frozen rows).";
      downloadButton.disabled = state.filteredRows.length === 0;
    }

    globalSearch.addEventListener("input", function () {
      state.globalQuery = globalSearch.value;
      state.page = 1;
      renderData();
    });

    pageSize.addEventListener("change", function () {
      state.pageSize = Number(pageSize.value) || 25;
      state.page = 1;
      tableShell.scrollTop = 0;
      renderData();
    });

    manuscriptButton.addEventListener("click", function () {
      state.manuscriptOnly = !state.manuscriptOnly;
      state.page = 1;
      renderData();
    });

    clearButton.addEventListener("click", function () {
      state.globalQuery = "";
      state.columnFilters = {};
      state.manuscriptOnly = false;
      state.page = 1;
      globalSearch.value = "";
      Object.keys(state.filterInputs).forEach(function (key) {
        state.filterInputs[key].value = "";
      });
      renderData();
    });

    downloadButton.addEventListener("click", function () {
      if (state.filteredRows.length === 0) return;
      downloadCSV(state.columns, state.filteredRows);
      status.textContent = "Prepared " + state.filteredRows.length +
        " filtered rows for download.";
    });

    fetchJSON(rootPath(app, "index.json")).then(function (index) {
      validateIndex(index);
      state.index = index;
      state.columns = index.columns;
      state.rows = index.rows;
      buildHeader();
      renderData();
    }).catch(function (error) {
      status.textContent = "Unable to load the supplementary table explorer.";
      metadata.textContent = error.message;
      tableBody.replaceChildren();
      downloadButton.disabled = true;
    });
  }

  function initialize() {
    var app = document.querySelector("[data-table-app]");
    if (app) initializeTableExplorer(app);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize);
  } else {
    initialize();
  }
}());

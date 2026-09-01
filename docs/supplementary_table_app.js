(function () {
  "use strict";

  var WAVE_COLUMNS = ["initial", "alpha", "delta", "omicron"];
  var VIEW_CONFIG = {
    sex: {
      value: "Sex-stratified (M/F)",
      label: "Sex-stratified (M/F)"
    },
    total: {
      value: "Total population",
      label: "Total population"
    }
  };

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

  function validateColumns(columns, label) {
    if (!Array.isArray(columns) || columns.length === 0) {
      throw new Error("The frozen table index has no " + label + " columns.");
    }
    var keys = columns.map(function (column) { return column.key; });
    if (new Set(keys).size !== keys.length) {
      throw new Error("The frozen table index has duplicated " + label + " columns.");
    }
    return keys;
  }

  function validateIndex(index) {
    if (!index || !Array.isArray(index.rows)) {
      throw new Error("The frozen table index is incomplete.");
    }
    var visibleKeys = validateColumns(index.columns, "visible");
    var downloadKeys = validateColumns(index.download_columns, "download");
    var requiredVisible = [
      "region_set",
      "geography_display",
      "people_vaccinated_per_hundred",
      "estimand_age_group",
      "initial",
      "alpha",
      "delta",
      "omicron"
    ];
    requiredVisible.forEach(function (key) {
      if (visibleKeys.indexOf(key) < 0) {
        throw new Error("The frozen table index is missing " + key + ".");
      }
    });
    visibleKeys.forEach(function (key) {
      if (downloadKeys.indexOf(key) < 0) {
        throw new Error("The download contract is missing " + key + ".");
      }
    });
    index.rows.forEach(function (row) {
      var supported = Object.keys(VIEW_CONFIG).some(function (viewKey) {
        return row.population_view === VIEW_CONFIG[viewKey].value;
      });
      if (!supported) {
        throw new Error("The frozen table contains an unsupported population view.");
      }
      downloadKeys.forEach(function (key) {
        if (!Object.prototype.hasOwnProperty.call(row, key)) {
          throw new Error("A frozen table row is missing " + key + ".");
        }
      });
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

  function createViewState(pageSize) {
    return {
      columnFilters: {},
      globalQuery: "",
      sortKey: null,
      sortDirection: "asc",
      page: 1,
      pageSize: pageSize
    };
  }

  function initializeTableExplorer(app) {
    var globalSearch = app.querySelector('[data-control="global-search"]');
    var pageSize = app.querySelector('[data-control="page-size"]');
    var viewButtons = Array.from(app.querySelectorAll("[data-view]"));
    var clearButton = app.querySelector('[data-action="clear"]');
    var downloadButton = app.querySelector('[data-action="download"]');
    var metadata = app.querySelector('[data-role="metadata"]');
    var tableHead = app.querySelector('[data-role="table-head"]');
    var tableBody = app.querySelector('[data-role="table-body"]');
    var tableShell = app.querySelector(".tablex-table-shell");
    var tableCaption = app.querySelector(".tablex-table caption");
    var status = app.querySelector('[data-role="status"]');
    var pagination = app.querySelector('[data-role="pagination"]');
    var initialPageSize = Number(pageSize.value) || 25;

    var state = {
      index: null,
      columns: [],
      downloadColumns: [],
      rows: [],
      filteredRows: [],
      filterInputs: {},
      activeView: "sex",
      viewStates: {
        sex: createViewState(initialPageSize),
        total: createViewState(initialPageSize)
      }
    };

    function currentViewState() {
      return state.viewStates[state.activeView];
    }

    function currentViewConfig() {
      return VIEW_CONFIG[state.activeView];
    }

    function columnByKey(key) {
      return state.columns.find(function (column) {
        return column.key === key;
      });
    }

    function rowsInActiveView() {
      var viewValue = currentViewConfig().value;
      return state.rows.filter(function (row) {
        return row.population_view === viewValue;
      });
    }

    function updateViewControls() {
      viewButtons.forEach(function (button) {
        button.setAttribute(
          "aria-pressed",
          button.dataset.view === state.activeView ? "true" : "false"
        );
      });
      tableCaption.textContent = currentViewConfig().label +
        " wave-specific P-scores by region, geography, and original age group";
    }

    function syncControlsFromView() {
      var viewState = currentViewState();
      globalSearch.value = viewState.globalQuery;
      pageSize.value = String(viewState.pageSize);
      Object.keys(state.filterInputs).forEach(function (key) {
        state.filterInputs[key].value = viewState.columnFilters[key] || "";
      });
      updateViewControls();
    }

    function updateSortIndicators() {
      var viewState = currentViewState();
      state.columns.forEach(function (column) {
        var header = tableHead.querySelector(
          'th[data-column="' + column.key + '"]'
        );
        if (!header) return;
        var active = viewState.sortKey === column.key;
        header.setAttribute(
          "aria-sort",
          active ? (viewState.sortDirection === "asc" ? "ascending" : "descending") : "none"
        );
        var indicator = header.querySelector(".tablex-sort-indicator");
        indicator.textContent = active
          ? (viewState.sortDirection === "asc" ? "▲" : "▼")
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
          var viewState = currentViewState();
          if (viewState.sortKey === column.key) {
            viewState.sortDirection = viewState.sortDirection === "asc" ? "desc" : "asc";
          } else {
            viewState.sortKey = column.key;
            viewState.sortDirection = "asc";
          }
          viewState.page = 1;
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
          var viewState = currentViewState();
          viewState.columnFilters[column.key] = filter.value;
          viewState.page = 1;
          renderData();
        });
        state.filterInputs[column.key] = filter;
        filterCell.appendChild(filter);
        filterRow.appendChild(filterCell);
      });

      tableHead.replaceChildren(headingRow, filterRow);
    }

    function filterRows() {
      var viewState = currentViewState();
      var globalTokens = searchTokens(viewState.globalQuery);
      var activeColumnFilters = Object.keys(viewState.columnFilters).filter(function (key) {
        return searchTokens(viewState.columnFilters[key]).length > 0;
      });
      var filtered = rowsInActiveView().filter(function (row) {
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
          var tokens = searchTokens(viewState.columnFilters[key]);
          var value = searchableValue(row, column);
          return tokens.every(function (token) {
            return value.indexOf(token) >= 0;
          });
        });
      });

      if (viewState.sortKey) {
        var sortColumn = columnByKey(viewState.sortKey);
        filtered = filtered.map(function (row, index) {
          return { row: row, originalIndex: index };
        }).sort(function (left, right) {
          var comparison = compareRows(
            left.row,
            right.row,
            sortColumn,
            viewState.sortDirection
          );
          return comparison === 0
            ? left.originalIndex - right.originalIndex
            : comparison;
        }).map(function (item) { return item.row; });
      }
      return filtered;
    }

    function renderMetadata(viewRows) {
      var geographyCount = new Set(state.filteredRows.map(function (row) {
        return row.region_set + "\r" + row.geography;
      })).size;
      var cards = [
        ["Table view", currentViewConfig().label],
        ["Frozen rows", viewRows.length.toLocaleString()],
        ["Matching rows", state.filteredRows.length.toLocaleString()],
        ["Geographies", geographyCount.toLocaleString()]
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
        currentViewState().page = target;
        tableShell.scrollTop = 0;
        renderData();
      });
      return button;
    }

    function renderPagination(pageCount) {
      var viewState = currentViewState();
      pagination.replaceChildren();
      pagination.appendChild(createPageButton(
        "First", 1, viewState.page === 1, false, "First page"
      ));
      pagination.appendChild(createPageButton(
        "Previous",
        Math.max(1, viewState.page - 1),
        viewState.page === 1,
        false,
        "Previous page"
      ));

      paginationItems(viewState.page, pageCount).forEach(function (item) {
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
            item === viewState.page,
            item === viewState.page,
            "Page " + item
          ));
        }
      });

      pagination.appendChild(createPageButton(
        "Next",
        Math.min(pageCount, viewState.page + 1),
        viewState.page === pageCount,
        false,
        "Next page"
      ));
      pagination.appendChild(createPageButton(
        "Last", pageCount, viewState.page === pageCount, false, "Last page"
      ));
    }

    function renderData() {
      var viewState = currentViewState();
      var viewRows = rowsInActiveView();
      state.filteredRows = filterRows();
      var pageCount = Math.max(
        1,
        Math.ceil(state.filteredRows.length / viewState.pageSize)
      );
      viewState.page = Math.min(Math.max(1, viewState.page), pageCount);
      var start = (viewState.page - 1) * viewState.pageSize;
      var end = Math.min(start + viewState.pageSize, state.filteredRows.length);
      renderBody(state.filteredRows.slice(start, end));
      renderPagination(pageCount);
      renderMetadata(viewRows);
      updateSortIndicators();
      updateViewControls();

      status.textContent = state.filteredRows.length === 0
        ? "Showing 0 of 0 matching rows in the " + currentViewConfig().label +
          " table (" + viewRows.length + " frozen rows in this table)."
        : "Showing " + (start + 1) + "–" + end + " of " +
          state.filteredRows.length + " matching rows in the " +
          currentViewConfig().label + " table (" + viewRows.length +
          " frozen rows in this table).";
      downloadButton.disabled = state.filteredRows.length === 0;
    }

    globalSearch.addEventListener("input", function () {
      var viewState = currentViewState();
      viewState.globalQuery = globalSearch.value;
      viewState.page = 1;
      renderData();
    });

    pageSize.addEventListener("change", function () {
      var viewState = currentViewState();
      viewState.pageSize = Number(pageSize.value) || 25;
      viewState.page = 1;
      tableShell.scrollTop = 0;
      renderData();
    });

    viewButtons.forEach(function (button) {
      button.addEventListener("click", function () {
        var requestedView = button.dataset.view;
        if (!VIEW_CONFIG[requestedView] || requestedView === state.activeView) return;
        state.activeView = requestedView;
        tableShell.scrollTop = 0;
        syncControlsFromView();
        renderData();
      });
    });

    clearButton.addEventListener("click", function () {
      var viewState = currentViewState();
      viewState.globalQuery = "";
      viewState.columnFilters = {};
      viewState.page = 1;
      syncControlsFromView();
      renderData();
    });

    downloadButton.addEventListener("click", function () {
      if (state.filteredRows.length === 0) return;
      downloadCSV(state.downloadColumns, state.filteredRows);
      status.textContent = "Prepared " + state.filteredRows.length +
        " filtered rows from the " + currentViewConfig().label +
        " table for download.";
    });

    fetchJSON(rootPath(app, "index.json")).then(function (index) {
      validateIndex(index);
      state.index = index;
      state.columns = index.columns;
      state.downloadColumns = index.download_columns;
      state.rows = index.rows;
      buildHeader();
      syncControlsFromView();
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

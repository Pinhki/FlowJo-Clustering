(function () {
  var timer = 0;
  var lastMode = new WeakMap();

  function visible(node) {
    return node && node.offsetParent !== null && getComputedStyle(node).visibility !== 'hidden';
  }

  function syncWorkspace() {
    document.querySelectorAll('.Pinhki-plot-workspace').forEach(function (workspace) {
      var left = workspace.querySelector(':scope > .Pinhki-plot-inputs');
      var right = workspace.querySelector(':scope > .Pinhki-plot-preview');
      var mode = '';
      if (window.innerWidth > 991 && visible(workspace) && visible(left) && visible(right)) {
        var leftHeight = left.scrollHeight;
        var rightHeight = right.scrollHeight;
        mode = leftHeight >= rightHeight ? 'float-plot-area' : 'float-left-panel';
      }
      if (lastMode.get(workspace) === mode) return;
      var pageY = window.scrollY;
      var leftY = left ? left.scrollTop : 0;
      var rightY = right ? right.scrollTop : 0;
      workspace.classList.remove('float-left-panel', 'float-plot-area');
      if (mode) workspace.classList.add(mode);
      if (left) left.classList.toggle('Pinhki-scroll-fade', mode === 'float-left-panel');
      if (right) right.classList.toggle('Pinhki-scroll-fade', mode === 'float-plot-area');
      lastMode.set(workspace, mode);
      requestAnimationFrame(function () {
        if (left) left.scrollTop = leftY;
        if (right) right.scrollTop = rightY;
        window.scrollTo({ top: pageY, left: window.scrollX, behavior: 'instant' });
      });
    });
    syncFades();
  }

  function soon() {
    clearTimeout(timer);
    timer = setTimeout(syncWorkspace, 180);
  }

  function updateFade(node) {
    var y = node.scrollHeight > node.clientHeight + 2;
    var x = node.scrollWidth > node.clientWidth + 2;
    node.classList.toggle('has-scroll-y', y);
    node.classList.toggle('has-scroll-x', x);
    node.classList.toggle('at-scroll-top', node.scrollTop <= 2);
    node.classList.toggle('at-scroll-bottom', node.scrollTop + node.clientHeight >= node.scrollHeight - 2);
    node.classList.toggle('at-scroll-left', node.scrollLeft <= 2);
    node.classList.toggle('at-scroll-right', node.scrollLeft + node.clientWidth >= node.scrollWidth - 2);
    node.classList.toggle('is-scroll-top', y && node.scrollTop > 2);
    node.classList.toggle('is-scroll-bottom', y && node.scrollTop + node.clientHeight < node.scrollHeight - 2);
  }

  function syncFades() {
    document.querySelectorAll('.cluster-marker-table-scroll .dataTables_scrollBody').forEach(function (node) {
      node.classList.add('Pinhki-bounded-scroll');
    });
    document.querySelectorAll('.Pinhki-bounded-scroll, .cluster-heat-scroll, .Pinhki-plot-workspace > .Pinhki-scroll-fade').forEach(updateFade);
  }

  document.addEventListener('scroll', function (event) {
    if (event.target instanceof Element && event.target.matches('.Pinhki-bounded-scroll, .cluster-heat-scroll, .Pinhki-plot-workspace > .Pinhki-scroll-fade')) updateFade(event.target);
  }, true);

  function inputId(grid, suffix) {
    return grid.id.replace(/cluster_heatmap_grid$/, suffix);
  }

  function sendOrder(grid, axis, values) {
    if (!window.Shiny) return;
    Shiny.setInputValue(inputId(grid, axis === 'row' ? 'heat_row_order' : 'heat_col_order'), values, { priority: 'event' });
  }

  function ordered(grid, axis) {
    return Array.from(grid.querySelectorAll(axis === 'row' ? '.cluster-heat-row-label' : '.cluster-heat-col-label'))
      .map(function (node) { return node.dataset.key; });
  }

  var dragged = null;
  var heatSortState = Object.create(null);
  document.addEventListener('dragstart', function (event) {
    var label = event.target.closest('.cluster-heat-axis-label');
    if (!label) return;
    dragged = label;
    event.dataTransfer.effectAllowed = 'move';
    event.dataTransfer.setData('text/plain', label.dataset.key || '');
  });
  document.addEventListener('dragover', function (event) {
    var target = event.target.closest('.cluster-heat-axis-label');
    if (!dragged || !target || target === dragged || target.dataset.axis !== dragged.dataset.axis) return;
    event.preventDefault();
    var grid = target.closest('.cluster-heatmap-grid');
    if (dragged.dataset.axis === 'row') {
      var sourceRow = dragged.closest('.cluster-heat-row');
      var targetRow = target.closest('.cluster-heat-row');
      targetRow.parentNode.insertBefore(sourceRow, targetRow.getBoundingClientRect().top > event.clientY ? targetRow : targetRow.nextSibling);
    } else {
      var keys = ordered(grid, 'col');
      var from = keys.indexOf(dragged.dataset.key), to = keys.indexOf(target.dataset.key);
      keys.splice(to, 0, keys.splice(from, 1)[0]);
      sendOrder(grid, 'col', keys);
    }
  });
  document.addEventListener('dragend', function () {
    if (!dragged) return;
    var grid = dragged.closest('.cluster-heatmap-grid');
    if (grid && dragged.dataset.axis === 'row') sendOrder(grid, 'row', ordered(grid, 'row'));
    dragged = null;
  });

  document.addEventListener('click', function (event) {
    var label = event.target.closest('.cluster-heat-axis-label');
    if (!label || label.dataset.dragged === '1') return;
    var grid = label.closest('.cluster-heatmap-grid');
    var axis = label.dataset.axis;
    var stateKey = grid.id + '|' + axis + '|' + label.dataset.key;
    var direction = heatSortState[stateKey] === 'asc' ? -1 : 1;
    heatSortState[stateKey] = direction === 1 ? 'asc' : 'desc';
    label.dataset.sort = direction === 1 ? 'asc' : 'desc';
    var values = [];
    if (axis === 'cluster') {
      grid.querySelectorAll('.cluster-heat-row[data-row-key]').forEach(function (row) {
        var key = row.dataset.rowKey || '';
        var numeric = Number(key);
        values.push({ key: key, value: numeric, text: key.toLowerCase() });
      });
      var allNumeric = values.length && values.every(function (x) { return Number.isFinite(x.value); });
      values.sort(function (a, b) {
        var result = allNumeric ? a.value - b.value : a.text.localeCompare(b.text, undefined, { numeric: true, sensitivity: 'base' });
        return direction * result;
      });
      sendOrder(grid, 'row', values.map(function (x) { return x.key; }));
    } else if (axis === 'col') {
      grid.querySelectorAll('.cluster-heat-row').forEach(function (row) {
        var cell = row.querySelector('.cluster-heat-cell[data-col-key="' + CSS.escape(label.dataset.key) + '"]');
        values.push({ key: row.dataset.rowKey, value: cell ? Number(cell.dataset.value) : NaN });
      });
      values.sort(function (a, b) { return direction * ((isNaN(a.value) ? Infinity : a.value) - (isNaN(b.value) ? Infinity : b.value)); });
      sendOrder(grid, 'row', values.map(function (x) { return x.key; }));
    } else {
      grid.querySelectorAll('.cluster-heat-cell[data-row-key="' + CSS.escape(label.dataset.key) + '"]').forEach(function (cell) {
        values.push({ key: cell.dataset.colKey, value: Number(cell.dataset.value) });
      });
      values.sort(function (a, b) { return direction * ((isNaN(a.value) ? Infinity : a.value) - (isNaN(b.value) ? Infinity : b.value)); });
      sendOrder(grid, 'col', values.map(function (x) { return x.key; }));
    }
  });

  function registerHandlers() {
    if (!window.Shiny || registerHandlers.done) return;
    registerHandlers.done = true;
    Shiny.addCustomMessageHandler('clusterHeatmap:reset', function (message) {
      var grid = document.getElementById(message.id);
      if (!grid) return;
      Object.keys(heatSortState).forEach(function (key) {
        if (key.indexOf(grid.id + '|') === 0) delete heatSortState[key];
      });
      sendOrder(grid, 'row', []);
      sendOrder(grid, 'col', []);
    });
  }

  document.addEventListener('DOMContentLoaded', soon);
  document.addEventListener('shiny:connected', function () { registerHandlers(); soon(); });
  document.addEventListener('shown.bs.tab', soon, true);
  window.addEventListener('resize', soon, { passive: true });
  window.addEventListener('load', soon);
  var observerRoot = document.body || document.documentElement;
  if (observerRoot) new MutationObserver(soon).observe(observerRoot, { childList: true, subtree: true });
  registerHandlers();
})();

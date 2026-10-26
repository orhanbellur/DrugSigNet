(function () {
  'use strict';
  function initializeTable(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = 'true';
    var payload = JSON.parse(root.querySelector('.ds-table-data').textContent);
    var rows = payload.rows.map(function (row, index) { return { row: row, index: index }; });
    var query = '', page = 0, size = payload.pageSize || 10, sortColumn = null, direction = 1;
    var search = root.querySelector('.ds-search'), selector = root.querySelector('.ds-page-size');
    var tbody = root.querySelector('tbody'), status = root.querySelector('.ds-table-status');
    var prev = root.querySelector('.ds-prev'), next = root.querySelector('.ds-next');
    var heads = Array.from(root.querySelectorAll('thead th'));
    selector.value = String(size);
    if (!selector.value) { size = 10; selector.value = '10'; }
    function text(value) { return value == null ? '' : String(value); }
    function matching() {
      var output = rows.filter(function (entry) { return !query || entry.row.some(function (v) { return text(v).toLocaleLowerCase().includes(query); }); });
      if (sortColumn !== null) output.sort(function (a, b) {
        var x = a.row[sortColumn], y = b.row[sortColumn];
        var missingX = x == null || x === '', missingY = y == null || y === '';
        if (missingX !== missingY) return missingX ? 1 : -1;
        var cmp = typeof x === 'number' && typeof y === 'number' ? x - y : text(x).localeCompare(text(y), undefined, {numeric: true});
        return cmp === 0 ? a.index - b.index : cmp * direction;
      });
      return output;
    }
    function cell(value, column) {
      var td = document.createElement('td'), content = text(value);
      if (typeof value === 'number' && /adjusted.?p|p[._ ]?adjust|p[._ ]?value|fdr/i.test(payload.columns[column])) content = value !== 0 && Math.abs(value) < 0.001 ? value.toExponential(2) : String(Number(value.toPrecision(3)));
      if (typeof value === 'number') td.className = 'ds-number';
      if (value == null || content === '') { td.textContent = '—'; return td; }
      if (content.length > 180) {
        var details = document.createElement('details'), summary = document.createElement('summary'), full = document.createElement('div');
        details.className = 'ds-cell-more'; summary.textContent = content.slice(0, 70) + '… (expand)'; full.textContent = content;
        details.append(summary, full); td.appendChild(details);
      } else td.textContent = content;
      return td;
    }
    function render() {
      var data = matching(), pages = Math.max(1, Math.ceil(data.length / size)); page = Math.min(page, pages - 1);
      tbody.replaceChildren();
      data.slice(page * size, (page + 1) * size).forEach(function (entry) {
        var tr = document.createElement('tr'); entry.row.forEach(function (v, column) { tr.appendChild(cell(v, column)); }); tbody.appendChild(tr);
      });
      if (!data.length) { var tr = document.createElement('tr'), td = document.createElement('td'); td.colSpan = payload.columns.length; td.textContent = 'No matching rows.'; tr.appendChild(td); tbody.appendChild(tr); }
      status.textContent = data.length ? 'Showing ' + (page * size + 1) + '–' + Math.min((page + 1) * size, data.length) + ' of ' + data.length + ' matching rows (' + rows.length + ' total)' : 'No matching rows (' + rows.length + ' total)';
      prev.disabled = page === 0; next.disabled = page >= pages - 1;
      heads.forEach(function (head, i) { head.setAttribute('aria-sort', i === sortColumn ? (direction === 1 ? 'ascending' : 'descending') : 'none'); });
    }
    search.addEventListener('input', function () { query = search.value.trim().toLocaleLowerCase(); page = 0; render(); });
    selector.addEventListener('change', function () { size = Number(selector.value); page = 0; render(); });
    prev.addEventListener('click', function () { page--; render(); }); next.addEventListener('click', function () { page++; render(); });
    heads.forEach(function (head, i) { head.querySelector('button').addEventListener('click', function () { direction = sortColumn === i ? -direction : 1; sortColumn = i; page = 0; render(); }); });
    root.querySelector('.ds-download').addEventListener('click', function () {
      function csv(value) {
        var out = text(value);
        if (typeof value !== 'number' && /^[=+@\t\r-]/.test(out)) out = "'" + out;
        return '"' + out.replace(/"/g, '""') + '"';
      }
      var lines = [payload.columns.map(csv).join(',')].concat(matching().map(function (entry) { return entry.row.map(csv).join(','); }));
      var blob = new Blob(['\ufeff' + lines.join('\r\n')], { type: 'text/csv;charset=utf-8;' });
      var url = URL.createObjectURL(blob), link = document.createElement('a'); link.href = url; link.download = root.id + '-' + root.querySelector('caption').textContent.replace(/[^A-Za-z0-9_-]+/g, '-').slice(0, 80) + '.csv'; link.click();
      setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
    });
    render();
  }
  function initialize() {
    document.querySelectorAll('.ds-table').forEach(function (root) {
      var details = root.closest('.ds-result');
      if (!details || details.open) initializeTable(root);
      else details.addEventListener('toggle', function () { if (details.open) initializeTable(root); });
    });
    var links = Array.from(document.querySelectorAll('.ds-nav a'));
    var sections = Array.from(document.querySelectorAll('.ds-section')), scheduled = false;
    function updateNavigation() {
      var current = sections[0];
      sections.forEach(function (section) { if (section.getBoundingClientRect().top <= 125) current = section; });
      links.forEach(function (link) { link.setAttribute('aria-current', current && link.hash === '#' + current.id ? 'true' : 'false'); });
      scheduled = false;
    }
    window.addEventListener('scroll', function () { if (!scheduled) { scheduled = true; requestAnimationFrame(updateNavigation); } }, {passive: true});
    window.addEventListener('resize', updateNavigation);
    updateNavigation();
    document.addEventListener('toggle', function () {
      if (window.Plotly) document.querySelectorAll('.js-plotly-plot').forEach(function (node) { window.Plotly.Plots.resize(node); });
    }, true);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initialize); else initialize();
})();

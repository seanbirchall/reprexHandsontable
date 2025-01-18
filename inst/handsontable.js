HTMLWidgets.widget({
  name: 'handsontable',
  type: 'output',
  factory: function(el, width, height) {
    var hotElement = document.createElement('div');
    el.appendChild(hotElement);
    return {
      renderValue: function(x) {
        if(el.hot && el.hot.destroy) {
          el.hot.destroy();
        }

        const inputId = el.id + '_action';

        function applyStyles(td, styles) {
          if(!styles) return;
          if (Array.isArray(styles)) {
            styles.forEach(style => {
              if (style) Object.assign(td.style, style);
            });
            return
          }
          Object.assign(td.style, styles);
        }

        function applyClasses(td, classes) {
          if(!classes) return;
          if (Array.isArray(classes)) {
            classes.forEach(className => {
              if (className) td.classList.add(className);
            });
            return
          }
          td.classList.add(classes);
        }

        // renderer
        function customRenderer(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          // column style
          if (Array.isArray(x.columnStyles)) {
            x.columnStyles.forEach(style => {
              if(style && style.col === col) {
                applyStyles(td, style.styles);
                applyClasses(td, style.classes);
              }
            });
          }
          // row style
          if (Array.isArray(x.rowStyles)) {
            x.rowStyles.forEach(style => {
              if(style && style.row === row) {
                applyStyles(td, style.styles);
                applyClasses(td, style.classes);
              }
            });
          }
          // cell style
          if (Array.isArray(x.cellStyles)) {
            x.cellStyles.forEach(style => {
              if(style && style.row === row && style.col === col) {
                applyStyles(td, style.styles);
                applyClasses(td, style.classes);
              }
            });
          }
         return td;
        }

        // config
        function getCellConfig(row, col) {
          let config = {
            renderer: customRenderer
          };

          // column readonly
          if (Array.isArray(x.readOnlyCols)) {
            x.readOnlyCols.forEach(read => {
              if(read) {
                config.readOnly = read.readonly;
              }
            });
          }
          // row readonly
          if (Array.isArray(x.readOnlyRows)) {
            x.readOnlyRows.forEach(read => {
              if(read) {
                config.readOnly = read.readonly;
              }
            });
          }
          // cells readonly
          if (Array.isArray(x.readOnlyCells)) {
            x.readOnlyCells.forEach(read => {
              if(read) {
                config.readOnly = read.readonly;
              }
            });
          }

          return config;
        }

        // build widget
        el.hot = new Handsontable(hotElement, {
          data: x.data,
          colHeaders: x.colHeaders || false,
          rowHeaders: x.rowHeaders || false,
          fixedRowsTop: x.fixedRowsTop || 0,
          fixedColumnsLeft: x.fixedColumnsLeft || 0,
          mergeCells: x.mergeCells || 0,
          licenseKey: x.licenseKey || 'non-commercial-and-evaluation',
          height: x.height || '100%',
          width: x.width || '100%',
          manualColumnResize: x.manualColumnResize || false,
          manualRowResize: x.manualRowResize || false,
          wordWrap: x.wordWrap || false,
          cells: function(row, col) {
            return getCellConfig(row, col);
          },
          afterInit: function() {
            this.selectCell(0, 0);
          },
          afterChange: function(changes, source) {
            if (!changes) return;
            Shiny.setInputValue(el.id + '_update', {
              update: changes
            });
          },
          afterSelection: function(r, c, r2, c2) {
            Shiny.setInputValue(el.id + '_select', {
              r: r + 1,
              c: c + 1,
              r2: r2 + 1,
              c2: c2 + 1
            });
          }
        });

        if (x.hideGridLines) {
          el.appendChild(style);
          el.appendChild(hotElement);
        }
      },
      resize: function(width, height) {
        if (el.hot && el.hot.updateSettings) {
          el.hot.updateSettings({
            width: width,
            height: height
          });
        }
      }
    }
  }
});

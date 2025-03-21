HTMLWidgets.widget({
  name: 'reprexHandsontable',
  type: 'output',
  factory: function(el, width, height) {
    var hotElement = document.createElement('div');
    el.appendChild(hotElement);
    return {
      renderValue: function(x) {

        if (typeof Handsontable === 'undefined') {
          console.error('Handsontable library not loaded!');
          return;
        }

        if (el.hot && el.hot.destroy) {
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
          readOnly: x.readOnly === true,
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
        const existingStyle = el.querySelector('.hideGridLines');
        if (existingStyle) {
          // If hideGridLines is false and we found a style element, remove it
          if (!x.hideGridLines) {
            existingStyle.remove();
          }
        } else if (x.hideGridLines === true) {
          // If hideGridLines is true and no style exists, add it
          const style = document.createElement('style');
          style.className = 'hideGridLines';  // More specific class name
          style.textContent = '.handsontable td { border: 0px !important; }';
          el.appendChild(style);
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

// Update table programmatically
if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler('handsontable-update', function(message) {
    // Find the Handsontable instance by ID
    var el = document.getElementById(message.id);
    if (!el || !el.hot) {
      console.error('Handsontable instance not found with ID:', message.id);
      return;
    }

    // Choose the appropriate method based on the message
    if (message.method === 'updateData') {
      el.hot.updateData(message.data, message.source);
    } else if (message.method === 'loadData') {
      el.hot.loadData(message.data, message.source);
    } else {
      console.error('Unknown method:', message.method);
    }
  });
}

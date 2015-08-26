<script>
var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-network-output');
    },
    renderValue: function(el, data) {
          //use join() to combine the array of strings in data that comprise the svg code
          //data is sent from shiny
          $(el).html(data.join(''));      
    }
  });
  Shiny.outputBindings.register(networkOutputBinding, 'timelyportfolio.networkbinding');

</script>

// Create and configure the renderer
var render = dagreD3.render();

var svg = d3.select("svg"),
    inner = d3.select("svg g"),
    zoom = d3.behavior.zoom().on("zoom", function() {
        inner.attr("transform", "translate(" + d3.event.translate + ")" +
                   "scale(" + d3.event.scale + ")");
    });
svg.call(zoom);
function drawDot (source) {
    var g = graphlibDot.read(source);
    // Set margins, if not present
    if (!g.graph().hasOwnProperty("marginx") &&
        !g.graph().hasOwnProperty("marginy")) {
        g.graph().marginx = 20;
        g.graph().marginy = 20;
    }

    g.graph().transition = function(selection) {
        return selection.transition().duration(500);
    };

    // Render the graph into svg g
     setTimeout(function(){ d3.select("svg g").call(render, g); }, 0);
}

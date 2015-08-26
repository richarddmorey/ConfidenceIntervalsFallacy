function make_figures()
{
	$(".article_figure").each(function(i){
		var a_element = $(this).children("a").first();
		var my_text = $( a_element ).attr("title");
		var my_src = $( a_element ).attr("href");
		$("<img/>", {
			"class": "article_thumbnail",
			src: my_src
		} ).appendTo( a_element );

		$("<div/>", {
			"class": "article_caption text-info",
			html: my_text
		} ).appendTo(this);	
	});
	//MathJax.Hub.Queue(["Typeset",MathJax.Hub]);	
}

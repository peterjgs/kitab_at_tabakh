# R Markdown


## ioslides: Footnotes

Add this after yaml header

```
<style>
div.footnotes {
  position: absolute;
  bottom: 0;
  margin-bottom: 10px;
  width: 80%;
  font-size: 0.6em;
}
</style>
```


```
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>

<script>
  $(document).ready(function() {
    $('slide:not(.backdrop):not(.title-slide)').append('<div class=\"footnotes\">');

    $('footnote').each(function(index) {
      var text  = $(this).html();
      var fnNum = (index+1).toString().sup();
      $(this).html(text + fnNum);

      var footnote   = fnNum + ': ' + $(this).attr('content') + '<br/>';
      var oldContent = $(this).parents('slide').children('div.footnotes').html();
      var newContent = oldContent + footnote;
      $(this).parents('slide').children('div.footnotes').html(newContent);
    });
  });
</script>
```


Example footnote:

```
<footnote content = "Dalgaard 2008, Introductory Statistics with R, Springer; O’Neill et al. 1983,  Am. Rev. Respir. Dis., 128:1051–1054."></footnote>
```


##  ioslides: Speaker's notes

Put notes in an html block:

```
<div class="notes">
This is my *note*.

- It can contain markdown
- like this list
</div>
```


Add ?presentme=true to the link for the slides file

Open the file with the augmented link (Chrome works, Safari disables pop-ups).

Press "p" in the top window. Run the slides from the top window, with the other window as the display window.





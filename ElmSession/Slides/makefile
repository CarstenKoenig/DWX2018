start: output/slides.html
	chromium-browser --new-window ./output/slides.html

output/slides.html: talk.md
	mkdir -p output
	pandoc -t revealjs --from markdown+tex_math_dollars --slide-level=2 -s -o output/slides.html talk.md -V revealjs-url=../../../reveal -V theme=beige --css="../custom.css"

clean:
	rm -rf output
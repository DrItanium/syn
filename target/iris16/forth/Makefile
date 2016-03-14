all: program
program: forth.obj
	iris16link -o forth.out forth.obj
forth.obj: forth.iris16
	iris16asm -o forth.obj forth.iris16

run: program
	iris16 forth.out

clean:
	rm forth.obj forth.out


CC = elm make
FILE = src/Main.elm
TMP = Senet.big.js
OUTPUT = Senet.js
OPTIM = --optimize

all: elmake
debug: elmake
optimize: elmtimize

elmake:
	$(CC) $(FILE) --output=$(OUTPUT)

elmtimize:
	$(CC) $(FILE) $(OPTIM) --output=$(TMP)
	uglifyjs $(TMP) --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $(OUTPUT)

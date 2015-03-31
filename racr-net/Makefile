

all: questionnaire test meta

questionnaire:
	mono-csc Questionnaire.cs Racr.cs -r:IronScheme.dll

test:
	mono-csc Test.cs Racr.cs -r:IronScheme.dll

meta:
	mono-csc Meta.cs

clean:
	rm Racr.exe

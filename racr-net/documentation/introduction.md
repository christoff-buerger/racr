_[>> Title <<](title.md) [>> Preface <<](synopsis.md) [>> Contents <<](contents.md) [>> Bibliography <<](bibliography.md)_
___

# Einleitung

Referenzattributgrammatiken \[[Hedin2000](bibliography.md)\] (RAG) und Graphersetzung \[[Rozenberg1997](bibliography.md),[Ehrig1999](bibliography.md)\] sind namhafte Konzepte im Übersetzerbau. Durch Referenzattribute lassen sich abstrakte Syntaxbäume (AST) deklarativ zu Graphen erweitern und analysieren. Sie eignen sich daher beispielsweise zur Entwicklung semantischer Analysen und Code-Generatoren. Üblicherweise kommen semantische Analysen und Graphersetzungen getrennt in unterschiedlichen Kompilierungsphasen zum Einsatz.

Zur Untersuchung der Vorteile einer engen Integration von Graphanalysen und -Ersetzungen wurde am Lehrstuhl Softwaretechnologie ein Verfahren namens Referenzattributgrammatik-gesteuerte Graphersetzung \[[Buerger2015b](bibliography.md)\] entwickelt, das effiziente, sich wechselseitig bedingende Graphanalysen und -Ersetzungen ermöglicht (RAG-gesteuerte Graphersetzung). Entscheidungen bezüglich einer Graphersetzung können unter Zuhilfenahme von Attributen getroffen werden. Graphersetzungen ändern die Struktur des AST, wobei die Neuauswertung nur jener Attributen ausgelöst wird, die von der Modifikation betroffen sind. Zur Neuauswertung von Attributen, die von Graphersetzungen beeinflusst werden, wird eine bedarfsgesteuerte, inkrementelle Evaluierungsstrategie eingesetzt, in welche innerhalb der Attributgleichung genommenen Ausführungspfade einfließen. Attributabhängigkeiten werden dabei nur betrachtet, wenn diese zur Laufzeit auftreten, also während der Auswertung einer Attributgleichung relevant sind. Dieser dynamische Ansatz steht im Gegensatz zu bekannten inkrementellen Auswertungsverfahren, aufbauend auf statischen Attributabhängigkeitsanalysen \[[Demers1981](bibliography.md),[MaddoxIII1997](bibliography.md)\].

Eine Referenzimplementierung der RAG-gesteuerten Graphersetzung liegt in Form einer _R6RS Scheme_-Bibliothek namens [RACR](https://github.com/christoff-buerger/racr) vor \[[Buerger2012](bibliography.md),[Buerger2015b](bibliography.md)\]. Sie umfasst Scheme-Prozeduren, um Grammatiken und Attribute zu spezifizieren, zur Syntaxbaum-Konstruktion, Traversierung, Abfrage von Knoteninformation und zur Attributsauswertung. Zusätzlich werden Prozeduren zur Graphersetzung bereitgestellt.

Die Vorteile der RAG-gesteuerten Graphersetzung wurden bereits in verschiedenen Bereichen demonstriert. Zum Beispiel wurde mittels _RACR_ eine [domänenspezifische Sprache zur Generierung interaktiver Fragebögen](https://github.com/christoff-buerger/racr/tree/master/examples/questionnaires) ([_Language Workbench Challenge 2013_](https://github.com/christoff-buerger/racr/blob/master/examples/questionnaires/documentation/language-workbench-challenge-2013.pdf), \[[Erdweg2013](bibliography.md)\]) spezifiziert. Des Weiteren wurde _RACR_ zur Implementierung von Laufzeit-Modellen für cyber-physische Systeme zur effizienten selbst-Optimierung hinsichtlich des Energiebedarfs eingesetzt \[[Buerger2015](bibliography.md)\]. Darüber hinaus werden in einer Lösung des _Transformation Tool Contest 2015_ \[[Mayerhofer2015](bibliography.md)\] mittels _RACR_ Transformationen von _fUML-Aktivitätsdiagrammen_ nach ausführbaren Petrinetzen realisiert. Die erzeugten Petrinetze werden hierbei inkrementell von _RACR_ ausgewertet, sodass eine sowohl effiziente als auch wohldefinierte Ausführungssemantik für _fUML-Aktivitätsdiagramme_ gegeben ist \[[Buerger2015a](bibliography.md)\].

## Aufgabenstellung

Um die Nützlichkeit des Verfahrens der RAG-gesteuerten Graphersetzung in zukünftigen Forschungsprojekten in der objektorientierten Programmierung untersuchen zu können, soll _RACR_ in dieser Arbeit innerhalb einer objektorientierten Sprache über eine entsprechende Schnittstelle verfügbar gemacht werden.

Damit die existierende _RACR_-Implementierung genutzt werden kann, muss die Möglichkeit geschaffen werden, eine _R6RS_-konforme virtuelle Maschine von der Host-Sprache aus anzusteuern. Dies beinhaltet, dass _Scheme_-Prozeduren von der Host-Sprache aus aufgerufen werden können, und umgekehrt (Callback-Funktionen). Die Funktionalitäten der prozeduralen Schnittstelle _RACRs_ müssen auf Objekte und Methoden abgebildet werden. Die entscheidenden Qualitätskriterien der Lösung sind eine benutzerfreundliche, objektorientierte Schnittstelle und geringe zusätzliche Laufzeitkosten im Vergleich zu einer reinen _Scheme_-Anwendung. Test-Anwendungen sind erforderlich, um die Korrektheit und eine akzeptable Performance der Schnittstelle zu gewährleisten.

Als Host-Sprache wurde _C#_ ausgewählt. Folglich soll eine _.NET_-Klassenbibliothek namens _RACR-NET_ geschaffen werden, mittels welcher der Funktionsumfang _RACRs_ von _C#_ aus über eine objektorientierte Schnittstelle nutzbar ist. Die Brücke zwischen _Scheme_ und _C#_ soll durch den Einsatz der [IronScheme-VM](http://ironscheme.codeplex.com/) geschaffen werden.

## Struktur der Arbeit

Die vorliegende Arbeit ist folgendermaßen gegliedert: [Kapitel 2](prerequisites.md) gibt einen Überblick über die RAG-gesteuerte Graphersetzung und andere verwendete Technologien. Die tatsächliche Umsetzung erfolgt in zwei Schritten. Zunächst wird eine prozedurale Schnittstelle entworfen, welche in [Kapitel 3](procedural-api.md) beschrieben wird. Die darauf aufbauende objektorientierte Schnittstelle sowie deren Implementierung werden in [Kapitel 4](object-oriented-api.md) vorgestellt. Anschließend wird das Ergebnis in [Kapitel 5](evaluation.md) getestet und ausgewertet. Die Arbeit endet mit einer Zusammenfassung und einen Ausblick in [Kapitel 6](consculsion.md).
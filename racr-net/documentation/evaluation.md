_[>> Title <<](title.md) [>> Preface <<](synopsis.md) [>> Contents <<](contents.md) [>> Bibliography <<](bibliography.md)_
___

# Evaluation

Es wurde bereits an dem Anwendungsbeispiel in [Kapitel 4.2](object-oriented-api.md) gezeigt, dass die an die _RACR-NET_-Schnittstelle gesetzten funktionalen Anforderungen von der vorgestellten Implementierung erfüllt werden, insbesondere bezüglich einer benutzerfreundlichen objektorientierten Schnittstelle. Der Zugriff auf Spezifikation und AST-Knoten erfolgt ausschließlich über Instanzen von `Racr.Specification` beziehungsweise `Racr.AstNode` und alle Aufrufe von _RACR_ sind in Methoden dieser Stellvertreter-Objekte gekapselt.

Im Folgenden soll gezeigt werden, dass _RACR-NET_ nicht nur benutzerfreundlich, sondern auch korrekt und effizient ist.

## Testen der Schnittstelle

Zum Testen von _RACR-NET_ wurde eine umfangreiche, [bereits existierende Anwendung](https://github.com/christoff-buerger/racr/tree/master/examples/questionnaires), die den wesentlichen Funktionsumfang _RACRs_ abdeckt, in _C#_ [reimplementiert](https://github.com/christoff-buerger/racr/tree/master/examples-net/questionnaires). Das Beispiel ist eine Lösung des [_Language Workbench Challenge 2013_](https://github.com/christoff-buerger/racr/blob/master/examples/questionnaires/documentation/language-workbench-challenge-2013.pdf) \[[Erdweg2013](bibliography.md)\], dessen Aufgabe darin bestand, eine domänenspezifische Sprache zu schaffen, mittels welcher interaktive Fragebögen zur Datenerfassung auf einfache Weise beschrieben und ausgewertet werden können. Die Anwendung bedient sich aller Mechanismen _RACRs_ mit Ausnahme von komplexeren Graphersetzungen. Um auch die funktionelle Korrektheit der Graphersetzungsmethoden sicherzustellen, wurden diese in einem eigenen [_NUnit_](http://www.nunit.org/)-Test erfasst.

## Performance-Messungen und -Vergleiche

Die Erzeugung von Adapter-Objekten und der indirekte Zugriff auf _RACRs_ Funktionalitäten über jene Objekte erzeugt einen Laufzeit-Overhead. Dieser wurde für eine _RACR_-Anwendung ermittelt, die im Folgenden beschrieben wird.

Unter Verwendung der in [Kapitel 4.2](object-oriented-api.md) gegebenen Sprachspezifikation wurden arithmetische Ausdrücke für verschiedene Konstantenbelegungen berechnet. Um die Ausführungszeiten des _C#_-Programms gegenüber denen des _Scheme_-Programms vergleichen zu können, wurde die Anwendung in beiden Sprachen, _Scheme_ (unter der Verwendung der _RACR_ _Scheme_-Bibliothek) und _C#_ (mittels der objektorientierten Schnittstelle), implementiert. Der Laufzeit-Overhead ist die Differenz der Ausführungszeiten beider Implementierungen in _IronScheme_. Die den zu berechnenden Ausdruck repräsentierenden ASTs wurden mithilfe eines _Python_-Skripts generiert und enthalten jeweils 5.000, 10.000 und 20.000 Binär-Operationen und ebenso viele Blatt-Knoten. Die Hälfte der Blatt-Knoten sind Konstanten, wobei insgesamt 26 verschiedene Konstanten-Definitionen benutzt werden.

Operationen	| Evals	| Rewrites	| _C#_		| _IronScheme_	| _Racket_
---------------:|------:|--------------:|--------------:|--------------:|--------:
5.000		| 1	| 0		| 1,43		| 1,36		| 0,45
10.000		| 1	| 0		| 2,93		| 2,92		| 0,98
20.000		| 1	| 0		| 6,21		| 6,03		| 2,17
5.000		| 1.000 | 1.000		| 72,44		| 72,20		| 24,31
10.000		| 1.000 | 1.000		| 150,14	| 145,81	| 71,08
20.000		| 1.000 | 1.000		| 330,58	| 316,97	| 217,79

**Tabelle 5.1:** Performance-Messungen (Laufzeit der letzten drei Spalten in Sekunden)

Alle Läufe wurden auf einem Rechner mit einem _Intel Core i5-3350P_ Vier-Kern-Prozessor und 16 GB RAM unter _Windows 8.1_ gemessen. Das verwendete _IronScheme_ Release war 115404, 32-Bit, vom 29. Oktober 2015. Als _.NET_ VM wurde das _Microsoft .NET Framework 4.0_ verwendet. Von zwanzig Messungen pro Lauf wurde die beste Zeit genommen. Tabelle 5.1 zeigt die Messergebnisse für eine einzelne Berechnung des Attributs `'Eval` (Zeile 1 bis 3). Ferner wurden innerhalb einer Schleife jeweils der Wert einer Konstanten in deren Definition modifiziert und anschließend `'Eval` für den Wurzel-Knoten ausgewertet (Zeile 4 bis 6).

Um die Performance von _IronScheme_ gegenüber anderen _Scheme_-VMs abschätzen zu können, wurden die Tests ebenfalls auf der [_Racket_](http://racket-lang.org/) _Scheme_-VM durchgeführt (_Racket Version 6.2_). Zur Erfassung des durch die objektorientierte Schnittstelle generierten Overheads sind jedoch lediglich die für _IronScheme_ gemessenen Ausführungszeiten relevant.

Die Messungen ergeben, dass die _RACR-NET_-Lösung erwartungsgemäß etwas langsamer ausführt als eine reine _Scheme_-Lösung. Der Performance-Overhead beträgt für 20.000 Knoten und 1.000 Auswertungen und Graphersetzungen circa 4,3% und ist damit durchaus akzeptabel. Des Weiteren wird ersichtlich, dass _IronScheme_ zwar langsamer ist als _Racket_, der Faktor jedoch keine Größenordnung beträgt (Im Gegensatz zu _Racket_ ist _IronScheme_ ein Ein-Man-Projekt, bei dem Performance nicht im Mittelpunkt steht).

Bei dem gewählten Beispiel handelt es sich um eine Referenzattributgrammatik, deren Attributsgleichungen auf simple Addition und Multiplikation zweier Fließkommazahlen beschränkt sind. Es bleibt daher zu untersuchen, wie stark der Geschwindigkeitsvorteil von _C#_ gegenüber _IronScheme_ in komplexeren Gleichungen zum Tragen kommt und, ob dieser den Overhead von _RACR-NET_ wohl möglich kompensiert.

Die Messergebnisse unterstreichen die Vorteile der RAG-gesteuerten Graphersetzung. Eine einmalige Auswertung von `'Eval` ist relativ teuer (1,43 Sekunden mittels _RACR-NET_), weil anfangs alle Attribut-Caches leer sind und so `'Eval` erst für jeden Teilausdruck berechnet werden muss. Da die vorgenommenen Graphersetzungen nur jeweils einen Anteil der zuvor berechneten Attributwerte invalidieren, begünstigt die inkrementelle Auswertung alle nachträglichen Berechnungen. Eintausend Berechnungen mit intermediären Graphersetzungen benötigen 330,58 Sekunden – nur circa 20% der tausendfachen Dauer der initiale Berechnung.

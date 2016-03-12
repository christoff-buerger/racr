_[>> Title <<](title.md) [>> Preface <<](synopsis.md) [>> Contents <<](contents.md) [>> Bibliography <<](bibliography.md)_
___

# Zusammenfassung und Ausblick

Abschließend wird der Beitrag dieser Arbeit noch einmal zusammengefasst und ein Ausblick auf offene Fragen und Erweiterungsmöglichkeiten gegeben.

## Eine objektorientierte Bibliothek für RAG-gesteuerte Graphersetzung

In dieser Arbeit wurde die Umsetzung einer objektorientierten Schnittstelle von _RACR_, der _Scheme_-Bibliothek zur Referenzattributgrammatik-gesteuerten Graphersetzung, für die Software-Plattform _.NET_ präsentiert. Diese Schnittstelle namens _RACR-NET_ ermöglicht es, _RACR_-Anwendungen in _C#_ zu programmieren.

_IronScheme_, eine _Scheme_-Implementierung für _.NET_, wurde als virtuelle Maschine eingesetzt, um innerhalb der Methoden der Schnittstelle die entsprechenden _RACR_-Prozeduren aufzurufen. Unter Verwendung der _IronScheme_-Klassenbibliothek wurde erst eine imperative Schnittstelle geschaffen. Darauf aufbauend entstand eine benutzerfreundliche, objektorientierte Schnittstelle, die den vollständigen Funktionsumfang _RACRs_ über Stellvertreter-Objekte für Spezifikationen und AST-Knoten abbildet.

In der Schnittstelle kommen keine _Scheme_-spezifischen Datentypen zum Einsatz, sodass Nutzer _RACR-NET_ ohne jegliche _Scheme_-Kenntnisse einsetzen können. Gleichzeitig bleibt die Nähe zu _RACRs_ originalen _Scheme_-Schnittstelle bewahrt. Der indirekte Zugriff auf AST-Knoten erzeugt in _RACR-NET_-Anwendungen einen Performance-Overhead gegenüber äquivalenten, via _IronScheme_ ausgeführten _RACR_-Anwendungen. Dieser wurde für eine Beispiel-Anwendung bestimmt und beträgt 4,3%.

## Zukünftige Arbeiten

_RACR-NET_ dient dem Zweck, die RAG-gesteuerte Graphersetzung von dem gewählten Technikraum _.NET_ aus nutzen zu können. Bisher wurde nur gezeigt, wie die Schnittstelle von _C#_ aus eingesetzt wird. Es existiert jedoch eine Vielzahl von _.NET_-Sprachen, innerhalb welcher _RACR-NET_ eingesetzt werden kann. Inwieweit dies für solche Sprachen effektiv umgesetzt werden kann, die neben Objektorientierung andere Programmierparadigmen verfolgen (zum Beispiel _F#_ mit der funktionale Programmierung), muss noch untersucht werden.

Ein interessanter Anwendungsfall besteht darin, bereits bestehende in _Scheme_ implementierte _RACR_-Anwendungen von _C#_ aus über _RACR-NET_ zu erweitern. Dies schließt die Möglichkeit ein, AST-Schemas um zusätzliche AST-Regeln zu ergänzen und weitere Attribute zu definieren. Um die Erweiterbarkeit einer via _Scheme_ spezifizierten Sprache in _C#_ zu ermöglichen und Kopplung und Vererbung von Attributgrammatiken über Sprachgrenzen hinaus zu realisieren, bedarf es weiterer Arbeit. Eine wichtige Aufgabe liegt dabei in der Sonderbehandlung des Falls, dass in _Scheme_ definierte Referenzattribute in _C#_ ausgewertet werden, da diese als AST-Knoten statt eines Stellvertreter-Objekts ein _RACR_-internes Objekt liefern. Der korrekte Umgang mit diesen Datentypen setzt _Scheme_-Kenntnisse voraus, die _RACR-NET_ in den jetzigen Anwendungsszenarien nicht verlangt.

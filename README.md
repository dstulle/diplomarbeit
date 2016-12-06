# Konzeption und Implementierung eines Übersetzungsschemas zur automatischen Verifikation von ReActor-Spezifikationen

Gemäß dem Mooreschen Gesetz verdoppelt sich alle 1-2 Jahre die Leistungsfähigkeit der Prozessoren. Lange geschah dies in Form einer Erhöhung der CPU-Taktfrequenz. In letzter Zeit geht der Trend in eine andere Richtung. Die Leistungsfähigkeit erhöht sich durch die Parallelisierung von Prozessen.

Diese Trendwende hat zur Folge, dass sich die Art und Weise wie Programme entwickelt werden darauf anpassen muss. Programme können nicht mehr nur aus strikt nacheinander auszuführenden Anweisungen bestehen. Statt dessen müssen Teile des Programmcodes parallel zueinander ausgeführt werden können.

Bei einer parallelen Ausführung von Programmen ist die Reihenfolge der ausgeführten Anweisungen nicht immer vorhersehbar. Dadurch entstehen neue Probleme und Herausforderungen. Unter anderem muss der Zugriff auf gemeinsame Ressourcen wie beispielsweise Bereiche im Arbeitsspeicher koordiniert werden, damit sich Prozesse nicht gegenseitig blockieren (Deadlock) oder durch ungeschicktes Timing Speicherinhalte verfälscht werden (Race Condition).

Probleme, die durch die gemeinsame Nutzung von Speicherbereichen oder der zeitlichen Ausführung von Operationen entstehen, können entweder durch schwer überschaubare Synchronisationsmechanismen vermieden werden, oder durch die Einhaltung einfacher Grundprinzipien umgangen werden.

Ein Rechenmodell zur Modellierung paralleler Prozesse ist das Aktormodell. Das Aktormodell nutzt zur Prozesskommunikation keinen gemeinsamen Speicherbereich (shared memory) sondern Nachrichten (message passing). Ein Aktor kann Nachrichten verarbeiten und senden sowie neue Aktoren erzeugen. Eine detailiertere Einführung in die Grundprinzipien des Aktormodells befindet sich in Abschnitt&nbsp;2.1.

Um ein Aktor-System bereits in der Entwurfsphase zu prüfen, ist es notwendig ein Aktor-System auf abstraktem Niveau formal zu spezifizieren. Eine Möglichkeit Aktor-Systeme deklarativ zu spezifizieren ist ReActor. Weitere Details zu ReActor in Kapitel&nbsp;4.

Komplexe Systeme haben meist eine hohe Zahl an möglichen Zuständen und Abläufen. Dabei reicht es meist nicht aus einzelne Situationen zu testen oder einige Abläufe zu simulieren, um die Funktionsfähigkeit fest zu stellen. Besonders bei sicherheitskritischen Systemen ist es wichtig das System vollständig zu analysieren. Ein formal spezifiziertes System kann durch ein automatisches Verfahren, genannt Model-Checking, verifiziert werden. Mehr zum Thema Modellprüfer (engl. Model Checker) wird im Abschnitt&nbsp;2.2 behandelt.

Ein Aktor-System kann mithilfe eines Modellprüfers verifiziert werden. Dazu muss ein allgemeines Ausführungsmodell für das zu prüfende Aktorsystem erstellt werden.

Um die Modellierung des Aktorsystems zu erleichtern, soll eine reine Spezifikation von Aktoren in ReActor in ein vollständiges Ausführungsmodell in TLA+ übertragen werden (engl. code transformation). In diesem vollständigen Ausführungsmodell wird die Spezifikation der Aktoren in die bereits vorhandene Spezifikation des allgemeinen Verhaltens der Aktoren eingebettet.

http://danielsturm.de/diplomarbeit-daniel.sturm.pdf

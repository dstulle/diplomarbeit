run: ActorExecutionModel.tla
	java tlc2.TLC -workers 1 -deadlock $<

tlatex: output ActorBase.inc ActorActions.inc ActorLifecycle.inc ActorMessages.inc ActorExecutionModel.inc Actors.inc EnvironmentActor.inc FactorialStepActor.inc Utils.inc

example: output EnvironmentActor-head.inc
clean:
	rm *.dvi *.inc output/*

output:
	mkdir -p output

%.tex: %.tla
	java tla2tex.TLA -textwidth 360 -shade -ptSize 11 -nops -metadir output $< 

%.inc: %.tex
	cat output/$< | sed -e '1,/begin{document}/ s/.*//' -e '/end{document}/,$$ s/.*//' | sed '/^$$/d' > $@


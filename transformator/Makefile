all: environment factorial-step

environment: input/environment.ral output
	./transformator.scm $@ > output/EnvironmentActor.tla

factorial-step: input/factorial-step.ral output
	./transformator.scm $@ > output/FactorialStepActor.tla

output:
	mkdir -p output

clean:
	rm -rf output

#%: input/%.ral
#	./transformator.scm $@ > output/$@.tla


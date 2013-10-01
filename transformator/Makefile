fac: environment-fac factorial-step factorial-actors
prod-cons: environment-prod-cons buffer producer consumer consumer-producer-actors

factorial-actors:
	./generate-actors-module.scm Environment FactorialStep > output/Actors.tla

consumer-producer-actors:
	./generate-actors-module.scm Environment Buffer Producer Consumer > output/Actors.tla


environment-fac: input-fac/environment.ral output
	./transformator.scm input-fac/environment.ral environment > output/EnvironmentActor.tla

factorial-step: input-fac/factorial-step.ral output
	./transformator.scm input-fac/$@.ral factorial-step > output/FactorialStepActor.tla


environment-prod-cons: input-prod-cons/environment.ral output
	./transformator.scm input-prod-cons/environment.ral environment > output/EnvironmentActor.tla

buffer: input-prod-cons/buffer.ral output
	./transformator.scm input-prod-cons/$@.ral buffer > output/BufferActor.tla

producer: input-prod-cons/producer.ral output
	./transformator.scm input-prod-cons/$@.ral producer > output/ProducerActor.tla

consumer: input-prod-cons/consumer.ral output
	./transformator.scm input-prod-cons/$@.ral consumer > output/ConsumerActor.tla


output:
	mkdir -p output

clean:
	rm -rf output

#%: input/%.ral
#	./transformator.scm $@ > output/$@.tla


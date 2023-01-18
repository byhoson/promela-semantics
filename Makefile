KOMPILE_FLAGS=--transition "print"
KOMPILE_BACKEND=java

kompile:
	kompile $(KOMPILE_FLAGS) --backend $(KOMPILE_BACKEND) promela.k

tests/hello.pml: kompile
	krun tests/hello.pml

tests/hello-init.pml: kompile
	krun tests/hello-init.pml

tests/hello-many.pml: kompile
	krun tests/hello-many.pml


SRCDIR:=src
DISTDIR:=dist

.PHONY: all
all:	$(DISTDIR)/PentominoSolver.class

.PHONY: $(DISTDIR)/PentominoSolver.class
$(DISTDIR)/PentominoSolver.class:
	javac -sourcepath $(SRCDIR) -d $(DISTDIR) -Xlint $(SRCDIR)/PentominoSolver.java

.PHONY: clean
clean:
	rm -rf $(DISTDIR)

.PHONY: run
run:
	@java -cp $(DISTDIR) PentominoSolver

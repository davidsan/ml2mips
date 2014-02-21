#
# MASTER STL M1 - Cours CA - 2005/2006
# 
# Makefile du typeur, de l'evaluateur et du compialteur d'un mini-ML
#

CAMLC=ocamlc
CAMLLINK=$(CAMLC) -o 
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDEP=ocamldep
NAME=mips

CIBLES=intertypeur intereval ml2$(NAME)

SRCS =  util.ml types.ml  alex.ml asyn.ml  typeur.ml env_typeur.ml intertypeur.ml eval.ml env_eval.ml intereval.ml env_trans.ml lift.ml langinter.ml trans.ml prod.ml comp.ml display.ml

IOBJS =  util.cmo types.cmo
SOBJS = alex.cmo  asyn.cmo  typeur.cmo env_typeur.cmo display.cmo

TOBJS = $(IOBJS) $(SOBJS)
EOBJS = $(TOBJS) eval.cmo env_eval.cmo 

COBJS = env_trans.cmo lift.cmo langinter.cmo trans.cmo prod.cmo comp.cmo
cible: $(CIBLES)

intertypeur: $(IOBJS) alex.ml asyn.ml  asyn.cmi $(TOBJS)
	$(CAMLLINK) $@ $(TOBJS) intertypeur.ml

intereval: $(IOBJS) alex.ml asyn.ml alex.cmi asyn.cmi $(TOBJS) env_eval.cmo
	$(CAMLLINK) $@ $(EOBJS) intereval.ml

ml2$(NAME): $(IOBJS) alex.ml asyn.ml alex.cmi asyn.cmi $(TOBJS) $(COBJS)
	$(CAMLLINK) $@ str.cma $(EOBJS)  $(COBJS) maincomp.ml

#java: prodjava.ml
#	rm prod.ml
#	ln -s prodjava.ml prod.ml

distrib:
	tar -cvf distrib.tar $(SRCS) alex.mll  asyn.mly asyn.mli Makefile alex.mll alex.mli asyn.mly asyn.mli prodjava.ml maincomp.ml runtime.java Makefile display.ml


.SUFFIXES:

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi 

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.mly.ml:
	$(CAMLYACC) $<

.mll.ml:
	$(CAMLLEX) $<

clean:
	/bin/rm -f *~ *.cmi *.cmo alex.ml asyn.ml $(CIBLES)

depend:
	cp Makefile Makefile.bak
	(sed -e '/^#.*DEPEND.*automatic/,$$d' Makefile.bak; \
	echo '### DEPENDENCIES: automatically generated. Do not edit'; \
	$(CAMLDEP) $(SRCS)) > Makefile

### DEPENDENCIES: automatically generated. Do not edit
util.cmo :
util.cmx :
types.cmo :
types.cmx :
alex.cmo : util.cmo asyn.cmi
alex.cmx : util.cmx asyn.cmx
asyn.cmo : util.cmo types.cmo asyn.cmi
asyn.cmx : util.cmx types.cmx asyn.cmi
typeur.cmo : util.cmo types.cmo
typeur.cmx : util.cmx types.cmx
env_typeur.cmo : util.cmo typeur.cmo types.cmo asyn.cmi alex.cmo
env_typeur.cmx : util.cmx typeur.cmx types.cmx asyn.cmx alex.cmx
intertypeur.cmo : util.cmo typeur.cmo types.cmo env_typeur.cmo asyn.cmi \
    alex.cmo
intertypeur.cmx : util.cmx typeur.cmx types.cmx env_typeur.cmx asyn.cmx \
    alex.cmx
eval.cmo : types.cmo
eval.cmx : types.cmx
env_eval.cmo : typeur.cmo types.cmo eval.cmo env_typeur.cmo
env_eval.cmx : typeur.cmx types.cmx eval.cmx env_typeur.cmx
intereval.cmo : typeur.cmo types.cmo intertypeur.cmo eval.cmo env_typeur.cmo \
    env_eval.cmo asyn.cmi alex.cmo
intereval.cmx : typeur.cmx types.cmx intertypeur.cmx eval.cmx env_typeur.cmx \
    env_eval.cmx asyn.cmx alex.cmx
env_trans.cmo : types.cmo
env_trans.cmx : types.cmx
lift.cmo : typeur.cmo types.cmo env_trans.cmo
lift.cmx : typeur.cmx types.cmx env_trans.cmx
langinter.cmo : types.cmo
langinter.cmx : types.cmx
trans.cmo : util.cmo typeur.cmo types.cmo langinter.cmo env_trans.cmo
trans.cmx : util.cmx typeur.cmx types.cmx langinter.cmx env_trans.cmx
prod.cmo : typeur.cmo types.cmo langinter.cmo env_typeur.cmo env_trans.cmo
prod.cmx : typeur.cmx types.cmx langinter.cmx env_typeur.cmx env_trans.cmx
comp.cmo : typeur.cmo types.cmo trans.cmo prod.cmo lift.cmo intertypeur.cmo \
    env_typeur.cmo env_trans.cmo display.cmo asyn.cmi alex.cmo
comp.cmx : typeur.cmx types.cmx trans.cmx prod.cmx lift.cmx intertypeur.cmx \
    env_typeur.cmx env_trans.cmx display.cmx asyn.cmx alex.cmx

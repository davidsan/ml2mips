#!/bin/bash
# Compile les ML du repertoire courant en assembleur MIPS
# Execute tout les fichiers assembleurs et redirige leur sortie
# vers un fichier d'extension .result
MIPS_PATH=../ # repertoire de MIPS
MIPS=ml2mips # nom du compilateur
SPIM=spim

if [ ! -e $MIPS_PATH/$MIPS ]; then
    ( cd $MIPS_PATH && make)
fi
PASSED=0
FAILED=0
TOTAL=0

LOG=""
for f in *.ml; do 
    asmf=${f%%.ml}.s # code assembleur mips
    resf=${f%%.ml}.result # resultat de l'execution du mips
    expf=${f%%.ml}.expected # resultat attendu

    # Compilation ML vers MIPS
    $MIPS_PATH/$MIPS $f
    # Execution du code MIPS et stockage du resultat dans .result
    $SPIM -f $asmf | tail -1 > $resf

    # Verification du resultat
    if [ "`diff $resf $expf`" == "" ];then
	printf `tput bold``tput setaf 2`"[PASSED]"`tput sgr0`"%10d $asmf\n" `cat $resf`
	PASSED=$((PASSED+1))
    else
	printf `tput bold``tput setaf 1`"[FAILED]"`tput sgr0`"%10d $asmf\n" `cat $resf`
	LOG=$LOG"\n"`diff -c $resf $expf`
	FAILED=$((FAILED+1))
    fi
    TOTAL=$((TOTAL+1))
done

if [ $FAILED -eq 0 ]; then
    printf `tput setaf 2``tput bold`
    printf "All $TOTAL tests passed.\n"
else
    printf `tput setaf 1``tput bold`
    printf "$TOTAL tests passed. $FAILED failures.\n"
    printf `tput sgr0`
    printf "$LOG\n"
fi
printf `tput sgr0`


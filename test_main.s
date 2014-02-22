


main:

  addiu $sp, $sp, -8
  sw    $fp, 4($sp)
  move  $fp, $sp


  li    $a0, 5
  li    $a1, 2
  li    $a2, 1
  li    $a3, 6

  
  jal   soustraction___1

  # Affichage de la valeur du programme ($v0)
  move  $a0, $v0
  li    $v0, 1
  syscall

  move  $sp, $fp
  lw    $fp, 4($sp)
  addiu $sp, $sp, 8

  # Termine l'exécution (exit)
  li    $v0, 10
  syscall
  nop

# Fin du fichier test.s

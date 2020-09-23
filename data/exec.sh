#! /bin/bash 

mode=1
width=180
height=188

rm -fr reference delta results
#convert  KRAFTWERK0.gif -coalesce -scale 250x188 output.gif
#mkdir images
#convert output.gif images/j%02d.png


martine -i images/j00.png -m $mode -dsk -o reference -onerow -oneline
for i in images/j*.png; do martine -i "$i" -m $mode -onerow -oneline -w $width -h $height -o results -pal reference/J000.PAL -json ; done
martine -delta -df results/\*.WIN -o delta -address "#d091" -m $mode -json -linewidth "#60"
rm -f delta/*_column.json
prepare_delta -sprite results/j00.json -delta delta/\*.json -out data.asm
#rasm krafwerk.asm -eo
#dsk -dsk krafwerk-mode2.dsk -list

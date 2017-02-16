start=1
end=$1
for i in $(seq -s' ' $start $end); do
  for n in {10..1}; do  
    TO="127.0.0.1:8080/sensor/node$i"
    R=$(( ( RANDOM % 10 )  + 1 ))
    RE=$(((n*10)-$R))
    DATA="{\"data\":\"$RE\"}"
    echo $DATA
    curl -i -H "Content-Type: application/json" -d $DATA $TO
    sleep 1
    echo $ENDPONT
  done
done 

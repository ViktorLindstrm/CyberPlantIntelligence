start=1
end=$1
token=$2
type=$3 #sensor,pump,led
echo $end
echo $token
for i in $(seq -s' ' $start $end); do
  for n in {10..1}; do  
    TO="localhost:8080/$type/$token/$3$i"
    R=$(( ( RANDOM % 10 )  + 1 ))
    RE=$(((n*10)-$R))
    DATA="{\"data\":\"$RE\"}"
    echo $DATA
    curl -i -H "Content-Type: application/json" -d $DATA $TO
    sleep 1
    echo $ENDPONT
  done
done 

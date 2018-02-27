#!/bin/bash

runscript() {
  sshpass -p 'habopen' ssh -q -T -p 8101 openhab@localhost 'list = tac -l && each ($list) { shell:echo $it && shell:echo "smarthome:voice interpret $it" > script.txt && shell:source script.txt && shell:echo } && shell:exec rm -f script.txt' < $1
}

echo === Setup ===
runscript setup.txt > /dev/null 2>&1

echo === Run ===
runscript demo-in.txt > demo-out.txt
runscript errata-in.txt > errata-out.txt

echo === Diff ===
diff demo-out.ref demo-out.txt

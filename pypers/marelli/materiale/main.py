# main.py

"Chiama due processi proc1a.py e proc1b.py"

import subprocess

CMD_a = ["python", "-c", "import proc1a; proc1a.main()"]
CMD_b = ["python", "-c", "import proc1b; proc1b.main()"]

if __name__ == "__main__":
    p_a = subprocess.Popen(CMD_a)
    p_b = subprocess.Popen(CMD_b)



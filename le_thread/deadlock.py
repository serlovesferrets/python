from threading import Thread, Lock

# Questo codice provoca un deadlock.

# Due lock sono creati.
destra, sinistra = Lock(), Lock()


def primo():
    # Destra viene acquisita da "primo"...
    destra.acquire()
    print("Primo accede a destra")

    # (stesso per sinistra)
    sinistra.acquire()
    print("Primo accede a sinistra")

    sinistra.release()
    destra.release()


def secondo():
    # "secondo" accede a sinistra...
    sinistra.acquire()
    print("Secondo accede a sinistra")

    # ...e quando "secondo" prova ad accedervi, il programma viene bloccato.
    destra.acquire()
    print("Secondo accede a destra")

    destra.release()
    sinistra.release()


prima_thread = Thread(target=primo)
seconda_thread = Thread(target=secondo)

prima_thread.start()
seconda_thread.start()

prima_thread.join()
seconda_thread.join()

print("Fine")

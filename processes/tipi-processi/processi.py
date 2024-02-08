import os
from multiprocessing import Process, current_process, Pipe

"""
# 1. Processi totalmente ignari:

In questo caso, i processi non sono consapevoli l'uno dell'altro e operano
indipendentemente senza alcuna comunicazione diretta o scambio di informazioni.
Esempio: Parallelismo di dati. Supponiamo di avere un'immagine divisa in pixel.
Ogni processo si occupa di elaborare i dati in un determinato set di pixel
senza avere alcuna consapevolezza degli altri processi che elaborano gli altri
pixel.
"""


def process_function(data):
    result = data * 2
    print(result)


def main_ignari():
    data_list = [1, 2, 3, 4]
    processes = []

    for data in data_list:
        p = Process(target=process_function, args=(data,))
        processes.append(p)
        p.start()

    for p in processes:
        p.join()


"""
# 2. Processi indirettamente a conoscenza

In questo caso, i processi potrebbero non conoscere direttamente gli altri, ma
possono interagire attraverso uno scambio indiretto di informazioni, ad
esempio, attraverso una struttura dati condivisa o un intermediario. Esempio:

Una coda condivisa. I processi possono mettere dati in una coda condivisa e
prelevarli successivamente. Anche se i processi non conoscono direttamente
gli altri processi, condividono una struttura dati (la coda) per scambiare
informazioni.
"""


def process_id():
    return f"Server PID: {os.getpid()}, Process Name: {current_process().name}, Process PID: {current_process().pid}"


def process_function(data, result_queue):
    print(process_id())
    print("Elabora: ", data)

    result = data * 2
    result_queue.put(result)


def main_indiretti():
    data_list = [1, 2, 3, 4]
    result_queue = Queue()
    processes = []

    for data in data_list:
        p = Process(target=process_function, args=(data, result_queue))
        processes.append(p)
        p.start

    for p in processes:
        p.join()

    print("Stampa dei risultati:")
    while not result_queue.empty():
        result = result_queue.get()
        print(result)


"""
# 3. Processi direttamente a conoscenza

In questo caso, i processi sono consapevoli l'uno dell'altro e possono
comunicare direttamente. Esempio: Comunicazione diretta. I processi comunicano
utilizzando messaggi diretti o altri meccanismi di comunicazione. Ad esempio,
uno scenario di scambio di dati tra server e client o tra processi all'interno
di un'applicazione che utilizza canali di comunicazione diretti.
"""


def process_function(conn):
    print(process_id())
    print("Elaboro il dato ricevuto ed invio il risultato")
    data_received = conn.recv()
    result = data_received * 2
    conn.send(result)
    conn.close()


def main_conoscenti():
    print(process_id())
    print("Creo una connessione e un processo figlio")

    parent_conn, child_conn = Pipe()

    p = Process(target=process_function, args=(child_conn,))
    p.start()

    data = 5
    parent_conn.send(data)
    result = parent_conn.recv()

    p.join()
    print(process_id())

    print("Stampo il risultato ricevuto")
    print(result)


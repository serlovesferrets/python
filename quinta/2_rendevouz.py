from threading import Semaphore, Thread

semaphore_1, semaphore_2 = Semaphore(0), Semaphore(0)

# Processi continuano quando entrambi sono verdi
# (corretto in classe)

def task_1():
    semaphore_1.release()
    print("lavoro 1")
    semaphore_2.acquire()
    print("finito: lavoro 1")


def task_2():
    semaphore_2.release()
    print("lavoro 2")
    semaphore_1.acquire()
    print("finito: lavoro 2")


def main():
    t1, t2 = Thread(target=task_1), Thread(target=task_2)

    t1.start()
    t2.start()

    t1.join()
    t2.join()

if __name__ == "__main__":
    main()

from random import randint

pagella: dict[str, list[tuple[str, tuple[int, int], tuple[int, int]]]] = {
    "Marco": [
        ("Matematica", (7, 2), (8, 1)),
        ("Italiano", (6, 3), (7, 2)),
        ("Inglese", (5, 1), (9, 0)),
    ],
    "Polo": [
        ("Matematica", (5, 6), (6, 3)),
        ("Italiano", (8, 1), (9, 0)),
        ("Inglese", (9, 2), (9, 3)),
    ],
    "Bruno": [
        ("Matematica", (10, 2), (9, 3)),
        ("Italiano", (6, 2), (7, 1)),
        ("Inglese", (7, 1), (8, 2)),
    ],
}

"""
1. Popola la struttura dati con i dati qui sopra.
2. Aggiungi alla struttura dati la pagella di un nuovo studente chiamato
   Albert Einstein, con 10 in tutte le materie e nessuna ora di assenza.
3. Aggiungi Fisica a tutte le pagelle
4. Stampa i dati sulla materia Matematica per un dato studente nel primo
   quadrimestre
5. Stampa i dati su una data materia e un dato studente nel
   secondo quadrimestre
6. Stampa la materia in cui ha fatto più assenze un dato studente.
7. Stampa il voto minimo e la materia (o materie) per un dato studente nel
   secondo quadrimestre
8. Stampa il voto massimo e la materia (o materie) per un dato studente
   indipendentemente dal quadrimestre, specificando se il voto è stato
   conseguito nel primo o nel secondo periodo
9. Stampa la media di tutte le materie nel primo quadrimestre per un
   dato studente
10. Stampa la media di tutti i voti senza distinguere gli studenti.
"""


def get_studente(prompt) -> str:
    student = ""
    while student not in pagella:
        print(prompt)
        student = input("Inserisci il nome di uno studente: ").title()
        if student not in pagella:
            print("Studente non trovato, scegli uno di: ")
            for stud in pagella.keys():
                print(f"- {stud}")
    return student


# Es 2
pagella["Albert Einstein"] = []
for subj in ["Matematica", "Italiano", "Inglese"]:
    pagella["Albert Einstein"].append((subj, (10, 0), (10, 0)))

# Es 3
for student, data in pagella.items():
    if student != "Albert Einstein":
        pagella[student].append(
            (
                "Fisica",
                (randint(5, 10), randint(0, 10)),
                (randint(5, 10), randint(0, 10)),
            )
        )
    else:
        pagella[student].append(("Fisica", (10, 0), (10, 0)))

# Es 4
for subj, (v1, h1), (v2, h2) in pagella[get_studente("Primo quad:")]:
    if subj == "Matematica":
        print(f"Voti e assenze quadrimestre #1: {v1}, {h1}")

# Es 5
for subj, (v1, h1), (v2, h2) in pagella[get_studente("Secondo quad:")]:
    if subj == "Matematica":
        print(f"Voti e assenze quadrimestre #2: {v2}, {h2}")

# Es 6
materie_e_ass: dict[str, int] = {}
student_ass = get_studente("Per assenze:")
for subj, (_, h1), (_, h2) in pagella[student_ass]:
    if subj not in materie_e_ass:
        materie_e_ass[subj] = 0

    materie_e_ass[subj] += h1 + h2

max_subj, max_hrs = "", 0
for subj, hrs in materie_e_ass.items():
    if hrs > max_hrs:
        max_subj, max_hrs = subj, hrs

print(f"La materia con più assenze ({max_hrs}) è {subj}.")


# Helper
quadrimestre = ""


def get_grade_by(student: str, default: int, by):
    subjs: set[str] = set()
    old_grade = default
    for subj, (v1, h1), (v2, h2) in pagella[student]:
        if old_grade == v1 or old_grade == v2:
            subjs.add(subj)
            continue
        if by(old_grade, v1):
            quadrimestre = "primo"
            subjs.clear()
            subjs.add(subj)
            old_grade = v1
        if by(old_grade, v2):
            quadrimestre = "secondo"
            subjs.clear()
            subjs.add(subj)
            old_grade = v2
    return (old_grade, subjs)


# Es 7
min_grade, subjs = get_grade_by(
    get_studente("Per voto minimo: "),
    1_000_000,
    lambda old, new: new < old,
)

print(f"Il voto più basso è {min_grade}, preso in: ")
for subj in subjs:
    print(f"- {subj}")

# Es 8
max_grade, subjs = get_grade_by(
    get_studente("Per voto massimo: "),
    -1,
    lambda old, new: new > old,
)

print(f"Il voto più alto è {min_grade}, preso in: ")
for subj in subjs:
    print(f"- {subj}")

print(f"Nel {quadrimestre} quadrimestre.")


# Es 9
total, count = 0, 0
for subj, (v1, _), (_, _) in pagella[get_studente("Per media del 1° quad:")]:
    total += v1
    count += 1

print(f"La media del primo quadrimestre è {total / count:.2f}.")

# Es 10
total, count = 0, 0
for vals in pagella.values():
    for subj, (v1, _), (v2, _) in vals:
        total += v1 + v2
        count += 2

print(f"La media totale è {total / count:.2f}.")

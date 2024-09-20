from statistics import mean

grades: dict[str, list[tuple[str, float, float]]] = {
    "Giuseppe Gullo": [
        ("Matematica", 9, 0),
        ("Italiano", 7, 3),
        ("Inglese", 7.5, 4),
        ("Storia", 7.5, 4),
        ("Geografia", 5, 7),
    ],
    "Antonio Barbera": [
        ("Matematica", 8, 1),
        ("Italiano", 6, 1),
        ("Inglese", 9.5, 0),
        ("Storia", 8, 2),
        ("Geografia", 8, 1),
    ],
    "Nicola Spina": [
        ("Matematica", 7.5, 2),
        ("Italiano", 6, 2),
        ("Inglese", 4, 3),
        ("Storia", 8.5, 2),
        ("Geografia", 8, 2),
    ],
}

grade_tuple = (10, 0)
grades["Albert Einstein"] = [
    ("Matematica", *grade_tuple),
    ("Italiano", *grade_tuple),
    ("Inglese", *grade_tuple),
    ("Storia", *grade_tuple),
    ("Storia", *grade_tuple),
]

grades["Giuseppe Gullo"].append(("Fisica", 9.5, 0))
grades["Antonio Barbera"].append(("Fisica", 8, 1))
grades["Nicola Spina"].append(("Fisica", 8, 3))
grades["Albert Einstein"].append(("Fisica", *grade_tuple))

all_grades: list[float] = []

for name, subject_data in grades.items():
    if name == "Giuseppe Gullo":
        for subject, grade, hours in subject_data:
            all_grades.append(grade)
            if subject == "Matematica":
                print(f"Matematica per Giuseppe: {grade}/10, ass.: {hours}")
    if name == "Nicola Spina":
        for subject, grade, hours in subject_data:
            all_grades.append(grade)
            if subject == "Inglese":
                print(f"{subject} per Giuseppe: {grade}/10, ass.: {hours}")
    if name == "Antonio Barbera":
        for subject, grade, hours in subject_data:
            all_grades.append(grade)
            if subject == "Geografia":
                print(f"Geografia per Antonio: {grade}/10")

nicola_grades = [grade for (_, grade, _) in grades["Nicola Spina"]]
nicola_mean = mean(nicola_grades)

print(f"Media per Nicola: {nicola_mean:.2f}")
print(f"Media totale: {mean(all_grades)}")

nicola_data = grades["Nicola Spina"]

max_hours = -1.0
max_subjs: list[str] = []

for subject, grade, hours in nicola_data:
    if hours > max_hours:
        max_hours = hours
        max_subjs.clear()

    if hours == max_hours:
        max_subjs.append(subject)

print(f"Materie in cui ha fatto più assenze Nicola:")

for subj in max_subjs:
    print(f"- {subj}")

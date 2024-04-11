from dataclasses import dataclass
from random import randint
from functools import reduce

# dataclass sono come record in altri linguaggi
# le preferisco per rappresentare collezioni di dati


@dataclass
class Activity:
    name: str
    time_taken: tuple[int, int, int]
    category: str


# 1. 9:36


data = {
    "giuseppe gullo": [
        Activity("corsa campestre", (40, 21, 0), "juniores mas"),
        Activity("corsa 100 mt", (0, 12, 0), "juniores mas"),
        Activity("corsa 200 mt", (0, 29, 19), "juniores mas"),
    ],
    "antonia barbera": [
        Activity("corsa campestre", (0, 39, 11), "juniores fem"),
        Activity("corsa 100 mt", (0, 18, 0), "juniores fem"),
        Activity("corsa 200 mt", (0, 0, 0), "juniores fem"),
    ],
    "nicola esposito": [
        Activity("corsa campestre", (0, 43, 22), "juniores mas"),
        Activity("corsa 100 mt", (0, 14, 0), "juniores mas"),
        Activity("corsa 200 mt", (0, 36, 19), "juniores mas"),
    ],
}

# 2. 9:36


data["giorgio sangion ortolan"] = [
    Activity("corsa campestre", (0, 28, 11), "juniores fem"),
    Activity("corsa 100 mt", (0, 28, 0), "juniores fem"),
    Activity("corsa 200 mt", (0, 28, 0), "juniores fem"),
]

# 3. 9:36


for k, v in data.items():
    cat = (
        v[0].category if len(v) != 0 else "incognita"
    )  # non dovrebbe mai entrare in else

    data[k].append(
        Activity(
            "corsa ad ostacoli 400 mt",
            (randint(0, 20), randint(0, 20), randint(0, 20)),
            cat,
        )
    )
# 4. 9:38


activity = data["giuseppe gullo"][0]
print(activity)

# 5. 9:42


activity = data["nicola esposito"][2]
print("---")
print(activity.name)
print(activity.time_taken)
print(activity.category)

# 6. 9:44


print("---")
time = data["antonia barbera"][1].time_taken
minutes, sec, millis = time
print(f"Tempo: {minutes}:{sec}.{millis}")

# 7. 9:53


print("---")
times = [
    act[2].time_taken 
    for act in data.values() 
    if act[2].category == "juniores mas"
]
print(f"Max time: {max(times)}")

# 8. 10:01

print("---")
acts = [
    (name, act[2])
    for name, act in data.items() 
    if act[2].category == "juniores mas"
]

min_act: Activity | None = None
min_name: str = "ulysses"

for name, act in acts:
    if min_act == None or act.time_taken < min_act.time_taken:
        min_act, min_name = act, name

if min_act == None:
    # Crasha il programma se min_act == None
    # Non può avvenire mai, però soddisfa il type checker
    raise Exception("Caso impossibile")

print(f"Min time: {min_act.time_taken} by {min_name}")

# 9. 10:17

print("---")
# Ho già i dati (punto 7, times)

totals = (0, 0, 0)
count = 0
for (m, s, ml) in times:
    count += 1
    (tm, ts, tml) = totals
    totals = (tm + m, ts + s, tml + ml)

(tm, ts, tml) = totals
mean = (
    tm / count if tm != 0 else tm, 
    ts / count if ts != 0 else 0, 
    tml / count if tml != 0 else 0
)

print(f"Media: {mean}")

# 10. 10:27

def activity_io(name: str) -> Activity:
    print(f"Per {name}:")
    # applica una funzione ad ogni elemento della lista
    [min, secs, millis] = map(
        lambda t: int(t),
        input("Inserisci min, secs e millis separati da una virgola: ").split(",")
    )
    cat = input("Inserisci la categoria: ")
    return Activity(name, (min, secs, millis), cat)

def add_athlete_io() -> None:
    athlete_name = input("Inserisci il nome dell'atleta: ")
    if athlete_name in data:
        print("Atleta esiste già!")
        return
    
    cats = ["corsa campestre", "corsa 100 mt", "corsa 200 mt", "corsa 400 mt"]
    acts = []
    for cat in cats:
        acts.append(activity_io(cat))
    
    data[athlete_name] = acts

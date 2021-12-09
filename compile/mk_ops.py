letters = [chr(letter_id) for letter_id in range(ord('a'),ord('z')+1)]

print("Provide a csv file with the ops, omit extension")
filename = input("Filename (ops): ")

if filename=="":
    filename="ops"

import csv

data = []

with open(filename+".csv", mode='r') as csv_file:
    csv_reader = csv.DictReader(csv_file, delimiter=',')
    for row in csv_reader:
        data.append(row)

number_list = []    # let I = 0
primfn2s_list = []  # let primfn2s 0 = "I"
emit_list = []      # let emit_primfn 0 = "C I"
rules_list = []     # let i x = (APPL (PRIM_FUN I x))

maybe_let = "let "

def next_number(name,n):
    return ("let "+name.upper()+" = "+str(n)+";")

def next_primfn2s(dname, n):
    return (maybe_let+"primfn2s "+str(n)+" = \""+dname+"\"")

def next_emit(dname, ptype, n):
    return (maybe_let+"emit_primfn "+str(n)+" = \""+ptype+" "+dname+"\"")

def rec_next_rule(uname, arity, swap_rank):
    if arity==0:
        return ("PRIM_FUN "+uname)
    else:
        middle=rec_next_rule(uname, arity-1, swap_rank-1)
        cur_letter = letters[arity-1]
        if swap_rank<=0:
            return ("APPL ("+middle+") " + cur_letter)
        else:
            return ("APPL " + cur_letter + " ("+middle+")")

def next_rule(name, arity, swap_rank):
    assert arity >= swap_rank >= 0, (name+": Arity < SwapRank or SwapRank < 0")
    prefix = "let "+name.lower()+" "+(" ".join([letters[i] for i in range(arity)]))+" = "
    return (prefix+rec_next_rule(name.upper(), arity, swap_rank)+";")

for n in range(0,len(data)):
    row=data[n]
    number_list.append(next_number(row["Name"], n))
    primfn2s_list.append(next_primfn2s(row["DispName"], n))
    emit_list.append(next_emit(row["DispName"], row["Type"], n))
    rules_list.append(next_rule(row["Name"], int(row["Arity"]), int(row["SwapRank"])))
    maybe_let = " /\\ "

def add_to_file(fname, content, end=""):
    for line in content:
        fname.write(line)
        fname.write('\n')
    if end != "":
        fname.write(end)
        fname.write("\n")
    fname.write("\n")

fl_file = open(filename+".fl",'wt')

add_to_file(fl_file, number_list)
add_to_file(fl_file, primfn2s_list, ";")
add_to_file(fl_file, emit_list, ";")
add_to_file(fl_file, rules_list)

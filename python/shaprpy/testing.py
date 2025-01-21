


from collections import OrderedDict
from rpy2.robjects.vectors import ListVector, FloatVector, StrVector

data = explanation["internal"].rx2("parameters")

# Sample data for demonstration
sample_listvector = ListVector({
    'a': FloatVector([1.1, 2.2, 3.3]),
    'b': StrVector(['one', 'two', 'three']),
    'c': FloatVector([4.4, 5.5, 6.6])
})

sample_listvector2 = ListVector({
    'aa': sample_listvector,
    'bb': sample_listvector
})

sample_listvector3 = ListVector({
    'aa': sample_listvector,
    'bb': sample_listvector,
    'cc': data.rx2(1),
    'dd': data.rx2(2),
    'ee': data.rx2(3),
    'ff': data.rx2(4),
    'gg': data.rx2(5),
    'hh': data.rx2(6),
    'ii': data.rx2(7),
    'jj': data.rx2(8),
    'kk': data.rx2(9),
    'll': data.rx2(10),
    'mm': data.rx2(11),
    'nn': data.rx2(12),
    'oo': data.rx2(13),
    'pp': data.rx2(14),
    'qq': data.rx2(15)
})

sample_listvector4 = ListVector({
    'aa': sample_listvector,
    'bb': sample_listvector,
    'cc': data.rx2(1),
    'dd': data.rx2(2),
    'ee': data.rx2(3),
    'ff': data.rx2(4),
    'gg': data.rx2(5),
    'hh': data.rx2(6),
    'ii': data.rx2(7),
    'jj': data.rx2(8),
    'kk': data.rx2(9),
    'll': data.rx2(10),
    'mm': data.rx2(11),
    'nn': data.rx2(12),
    'oo': data.rx2(13),
    'pp': data.rx2(14),
    'qq': data.rx2(15),
    'rr': data.rx2(16)
})


data[15]
data2 = explanation["internal"].rx2["parameters"].rx2["causal_ordering"]

data2 = explanation["internal"]

data22=recurse_r_tree(data2)

data22.items()

data22.keys()

data22["iter_list"].keys()

data22["iter_list"]["element_1"].keys()



data22_keys = 
print(f"The keys of data22 are: {list(data22_keys)}")

aa=base.list(a=1,b=2,c=3)
aa=base.list(1,2,3)

if type(aa.names) == type(NULL):
    aa.names = [f"element_{i+1}" for i in range(len(aa))]
aa    


type(data.names) == type(NULL)


length_aa = len(aa)
print(f"The length of aa is: {length_aa}")

def assign_names_to_listvector(lv, prefix='element'):
    if lv.names is None:
        lv.names = [f"{prefix}_{i+1}" for i in range(len(lv))]
    for i, name in enumerate(lv.names):
        if isinstance(lv[i], ListVector):
            assign_names_to_listvector(lv[i], prefix=f"{prefix}_{i+1}")

# Assign names to aa
assign_names_to_listvector(aa)

base.length(aa)

length(aa)

base.names(aa)

ListVector({'Unnamed': 1, 'sd': 2, 'Unsdamed': 3})

def set_ListVector_names(data):
    for i, content in enumerate(data):
        if isinstance(content, ListVector):
            if type(content.names) == type(NULL):
                data.rx2[i+1] = ListVector({'Unnamed': data.rx2(i+1)})
            set_ListVector_names
    return data


data2 = set_ListVector_names(data)

for i, content in enumerate(data):
    if isinstance(content, ListVector):
        print(i, content.names)

def looper(data):
    if(type(data) == ListVector):
        if type(data.names) == type(NULL):
           data = ListVector({'Unnamed': data})
        for d in data:

            looper(d)
    else:
        print(data.names, data)


        for d in data:
            print(d)
    print(d.names)
            if(type(d) == ListVector):


    for d in data:

looper(data)

data.rx2(29).rx2["Unnamed"]
data.names[29]
data.rx2[29]

sample_listvector4.rx2[15] = ListVector({'Unnamed': sample_listvector4.rx2(17)})

aaa=recurse_r_tree(sample_listvector4)

# Get names of the dict
type(sample_listvector4.rx2["aa"].names) == type(NULL)
type(sample_listvector4.rx2["rr"].names) == type(NULL)

ListVector({**dict(zip(["aa"],sample_listvector4.rx2["rr"].items()))})
ListVector('aa'={**dict(sample_listvector4.rx2["rr"].items())})

ccc=ListVector({'Unnamed': sample_listvector4.rx2["rr"]})
ccc.names




bbb=recurse_r_tree(sample_listvector4)



def assign_names_to_listvector(lv, prefix='element'):
    if lv.names is not :
        for i, name in enumerate(lv.names):
            if name is None or name == '':
                lv.names[i] = f"{prefix}_{i+1}"
            if isinstance(lv[i], ListVector):
                assign_names_to_listvector(lv[i], prefix=f"{prefix}_{i+1}")

# Assign names to sample_listvector3
assign_names_to_listvector(sample_listvector3)


aaa=recurse_r_tree(sample_listvector3)


print(data)

from collections import OrderedDict
from rpy2.robjects.vectors import ListVector, FloatVector, StrVector

OrderedDict(zip(data.names, [recurse_r_tree(d) for d in data]))

data.names.index

routput.rx2('iterative_results')

explanation["internal"].rx2("parameters")[0:3]
explanation["internal"].rx2["parameters"]

recurse_r_tree(first_five_elements[:3][0:2])

###############


dict(zip(data.names, [recurse_r_tree(d) for d in data])

data_length = len(data)
print(f"The length of data is: {data_length}")

for element in data:
    recurse_r_tree(element)








###################




explanation.keys()

explanation["internal"].keys()

explanation["saving_path"]



explanation["rinternal"].rx2["objects"].rx2["feature_specs"]
recurse_r_tree(explanation["rinternal"].rx2["objects"])

.rx2["coal_feature_list"])

recurse_r_tree(explanation["internal"])

recurse_r_tree(explanation["routput"].rx2("internal"))
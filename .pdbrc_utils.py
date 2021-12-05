
def view_item(item):
    import pprint
    if type(item)==dict or not hasattr(item, '__dict__'):
        print('\033[95m')
        pprint.pp(item)
        print('\033[0m')
    else:
        print('\033[92m')
        pprint.pp(vars(item))
        print('\033[0m')

def view_table(item):
    import pprint
    pprint.pp([vars(a) for a in item.objects.all()])

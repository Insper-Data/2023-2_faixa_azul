
import pandas as pd
import pathlib

lentidao = pd.read_csv('links_lentidao_por_ano.csv')

dict_lentidao = pd.Series(lentidao.link.values,index=lentidao.nome).to_dict()


DATA_DIR = pathlib.Path.cwd() / 'dados'
DATA_DIR.mkdir(exist_ok=True, parents=True)

df = pd.concat([pd.read_csv(DATA_DIR / arquivo,encoding='latin-1', sep=';') for arquivo in dict_lentidao.keys()])
df.to_csv(DATA_DIR / 'lentidao.csv', index=False)

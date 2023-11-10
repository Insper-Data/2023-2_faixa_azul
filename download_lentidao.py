import pandas as pd
import pathlib
import json
import requests
import io

dados = json.load(open('links_lentidao.json'))

DATA_DIR = pathlib.Path.cwd() / 'dados'
DATA_DIR.mkdir(exist_ok=True, parents=True)

lentidao = []
if not (DATA_DIR / "lentidao.csv").exists():
    for nome_arquivo, url in dados.items():
        path_dados = DATA_DIR / nome_arquivo
        print(f'Fazendo download de {nome_arquivo}')
        headers = {
        'User-Agent': \
            'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) ' \
            'AppleWebKit/537.36 (KHTML, like Gecko) ' \
            'Chrome/39.0.2171.95 Safari/537.36',
        }
        response = requests.get(url, headers=headers)
        lentidao.append(
            pd.read_csv(
                io.StringIO(response.content.decode('latin')), 
                encoding='latin-1', sep=';')
        )

    df = pd.concat(
        [base.rename(
            {"Trecho": "descricao", 
             "Trechos": "descricao", 
             "Data": "data", 
             "tamanho (metros)": "tamanho", 
             "data/hora": "data"}, axis = 1) 
        for base in lentidao])
    df.to_csv(DATA_DIR / 'lentidao.csv', index=False, encoding='latin-1')
else: print(f'Os dados já estão baixados')



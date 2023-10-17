import requests
import pathlib
import os

## dicionario com os nomes dos arquivos e suas respectivas urls
dados = { 'acidentes_naofatais.csv' : 'http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_naofatais.csv', \
            'obitos_publico.csv' : 'http://painelderesultados.infosiga.sp.gov.br/bases/obitos_publico.csv' }

DATA_DIR = pathlib.Path.cwd() / 'dados'
DATA_DIR.mkdir(exist_ok=True, parents=True)

for nome_arquivo, url in dados.items():
    path_dados = DATA_DIR / nome_arquivo
    if not path_dados.exists():
        print(f'Fazendo download de {nome_arquivo}')
        headers = {
        'User-Agent': \
            'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) ' \
            'AppleWebKit/537.36 (KHTML, like Gecko) ' \
            'Chrome/39.0.2171.95 Safari/537.36',
        }
        response = requests.get(url, headers=headers)
        csv_content = response.content.decode(encoding='latin-1')
        with open(path_dados, 'w', encoding='latin-1') as file:
            file.write(csv_content)
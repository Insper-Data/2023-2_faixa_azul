import pandas as pd
import numpy as np
import pathlib
import json 
import os
from download_infosiga import download_infosiga

if not (pathlib.Path.cwd() / "cruzamento_infosiga-lentidao.json").exists():
    avenidas = (pd.read_csv("dados/acidentes_naofatais.csv", encoding = "latin", sep = ";")
    .rename({
        "Data do Acidente": "data",
        "Logradouro": "rua",
        "Município": "nm_municipio",
        "Veículos Envolvidos - Motocicleta": "motocicletas",
        "Pessoas Envolvidas - Leve": "leve",
        "Pessoas Envolvidas - Grave": "grave",
    }, axis = 1)
    .filter(["data", "nm_municipio", "rua", "grave", "leve", "motocicletas"])
    .query(f"(grave > 0 or leve > 0) and motocicletas > 0 and nm_municipio == 'SAO PAULO'")
    .assign(
        mes = lambda _: _.data.str[5:7], 
        ano = lambda _: _.data.str[0:4])
    .replace({"rua": {"'": " "}}, regex = True)
    .groupby(["ano", "rua"])
    .aggregate({"motocicletas": "count"})
    .reset_index()
    .sort_values(by = ["ano", "motocicletas"])
    .query("ano == '2021' and ~ rua.str.contains('SP ') and ~ rua.str.contains('SPA ') and ~ rua.str.contains('BR ')")
    .tail(80)
    )

    lentidao = (
        pd.read_csv("dados/lentidao.csv", encoding="latin-1", sep = ",")
        .replace({"corredor": {"Av": "Avenida", ",": "", "/": " ", "\(": " ", "\)": " ", "\.": " "}}, regex = True)
    )

    banlist = ["AVENIDA", "ESTRADA", "DOS", "DE", "DO", "DA", "RUA"]

    dicio_regex = {
        avenida: "|".join([palavra for palavra in avenida.split(" ") if palavra not in banlist and len(palavra) > 1]) 
        for avenida in avenidas.rua.to_list()
    }

    dicio_conversao = {}
    for avenida, regex in dicio_regex.items():
        valores = lentidao.query(f"corredor.str.upper().str.contains('{(regex)}')").corredor.unique()
        if len(valores) == 0:
            dicio_conversao.update({avenida: None})
        else:
            print(avenida, "->", regex)
            print(dict({0: "Sair", 1: "Nenhuma das opções"}, **{str(numero + 2): valores[numero] for numero in range(len(valores))}))
            try: resposta = int(input())
            except:
                print("ERRO")
                resposta = int(input())
            if resposta == 0: break
            elif resposta == 1: dicio_conversao.update({avenida: None})
            else: dicio_conversao.update({avenida: valores[resposta - 2]})


    with open("cruzamento_infosiga-lentidao.json", "w") as outfile:
        outfile.write(json.dumps(dicio_conversao))

if not (pathlib.Path.cwd() / "dados" / "acidentes_naofatais.csv").exists():
    download_infosiga()

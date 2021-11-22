# Análise das bolsas concedidas pelo CNPq

## Introdução

Este aplicativo foi desenvolvido como trabalho de conclusão de curso do curso de dashboards promovido pela [Curso-R](https://curso-r.com/cursos/dashboards/). O objetivo deste aplicativo é fornecer ao usuário uma maneira amigável de explorar os dados relativos ao pagamento de bolsas e auxílios à pesquisa científica realizados pelo [Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq)](https://www.gov.br/cnpq/pt-br). Utilizando esta interface, o usuário terá acesso a ferramentas para filtrar a quantidade de bolsas cedidas e o valor pago, em relação à período, área de pesquisa, bem como localização e instituição de destino do auxílio.

## Como utilizar

Ao iniciar o app, nenhum filtro específico é aplicado, com excessão da amplitude temporal, que pode ser alterada manualmente (é uma forma de diminuir a quantidade de dados carregados e acelerar a inicialização do aplicativo). Você pode filtrar os resultados selecionando um ou mais critérios específicos na barra de controle à sua direita. Os filtros funcionam como "funil", ou seja, ao escolher um país específico, por exemplo, apenas as cidades e instituições deste país estarão disponíveis nos filtros subsequentes. De mesmo modo, ao selecionar uma grande área de pesquisa, somente as áreas "filhas" poderão ser escolhidas. Todas as seleções e pré-seleções podem ser alteradas pelo usuário.

### Mapa das bolsas

Este mapa mostra a distribuição das cidades de destino das bolsas. Ao aproximar a visão do mapa, um marcador indicará cada cidade para qual houve a destinação de alguma bolsa. Ao clicar no marcador, você poderá ver o nome da cidade e a quantidade de bolsas cedidas. Caso nenhum filtro esteja selecionado, serão apresentados os resultados para todas as categorias, modalidades e áreas.

## Aviso

O autor deste aplicativo não possui ligação com o Ministério da Ciência, Tecnologia e Inovações ou com qualquer outro órgão do Governo Federal. Os dados aqui apresentados foram obtidos diretamente do [Portal de Dados Abertos do CNPq](http://dadosabertos.cnpq.br/pt_BR/organization/cnpq), portanto, quaisquer eventuais erros e discordâncias são de responsabilidade do Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq). As manipulações feitas na base de dados visaram tão somente facilitar sua apresentação no formato aqui disponível. Nenhum dado original foi alterado e todas as transformações feitas podem ser encontradas no [repositório deste aplicativo](https://github.com/marwincarmo/shiny-bolsas).

## Correções

Se você identificar algum erro ou tiver sugestões de mudança, por favor, crie uma issue no repositório fonte. Ou entre em contato por email ou outra rede social. Os endereços podem ser encontrados na página *linkada* no rodapé.
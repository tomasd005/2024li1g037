# Immutable Towers - Sistema UI

Tags: #sistema #ui

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[backlog-jogo]]

## Responsabilidades

- HUD superior
- painel lateral direito
- sidebar da loja
- mensagens temporarias
- overlays de pausa, vitoria e derrota
- menus principais e secundarios

## Modulos principais

- `app/Desenhar.hs`
- `app/UIComponents.hs`
- `app/UIRects.hs`
- `app/UIState.hs`
- `app/UIText.hs`
- `app/Eventos.hs`

## Decisoes atuais

- a UI usa um espaco virtual base de `1920x1080`
- o desenho final e escalado com `uiScale`
- a sidebar da loja fica fixa do lado esquerdo
- o painel de detalhes da torre fica fixo do lado direito
- a UI bloqueia input no mapa por baixo atraves de hitboxes explicitas

## Pontos sensiveis

- qualquer alteracao de layout deve atualizar tambem o input em `Eventos.hs`
- `UIRect` e a base da coerencia entre desenho e clique
- a loja e o painel lateral nao devem tapar leitura critica do mapa

## Proximos melhoramentos

- validar ergonomia da sidebar em mais playtests
- criar scroll/categorias se a loja crescer
- unificar ainda mais as hitboxes de render/input

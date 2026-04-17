# AA-drevesa
## Uvod
1. Motivacija
    - quick-sort -> bubble sort
    - drevesa (AVL, rdeče-črne) -> verižni seznam

## Algoritmi
1. 2-3 drevesa
    - **Psevdo-vozlišče** (vsebuje eno ali dve binarni vozlišči). Povezave znotraj psevdo-vozlišč so **vodoravne**, povezave med psevdo-vozlišči so **navpične**.
    - Le desne povezave smejo biti vodoravne.
2. Če želimo drevo uravnotežiti, rabimo v vsakem vozlišču še neko dodatno informacijo
    - RD-drevesa - barva.
    - 2-3-drevesa - ali ima vozlišče vhodno ali izhodno vodoravno povezavo.
    - AA-drevesa - **nivo** - navpična višina vozlišča (spodnja vozlišča imajo nivo 1).
3. Uravnoteženje naredimo s pomočjo pomožnih operacij, ki jih ponavadi imenujemo *rotate* in *split*. Pri teh operacijah je treba obravnavati veliko število možnosti, ki pridejo zaradi oblike psevdo-vozlišča. Zato, da zmanjšamo to število, vpeljemo pravilo
> Preden gremo preveriti število vozlišč v psevdo-vozlišču poglejmo, če so vse vodoravne povezave kažejo v desno.
- Dobimo le 2 možnosti
- Le eno od teh bo treba posodobiti
4. Za binarno vozlišče $p$ definiramo operacije `skew` in `split`.
5. Opis `insert` in `delete`
    - Primer `insert`
    - Primer `delete`

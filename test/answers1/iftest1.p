0: LDA 5
1: STA i
2: LDA i
3: JMN L0
4: JMZ L0
5: LDA 1
6: STA return
7: LDA return
8: JMP L1
9: LDA 0
10: STA return
11: LDA return
12: HLT

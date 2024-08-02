import { findPositions, filterKeys } from "./findpositions";
import { eventNameToString } from "./mesh";

const letters = 'ABCDEFGHIJKLMNOP';
const numbers = '123456789';

export class AnalyzerNode {
    tokens = {
        series:[],
        letters:{},
        fromLetters: {},
        fromDigits: {},
        numbers:{},
        eventObjects:{}
    }

    log = []

    constructor(log) {
        this.log = log;
        const tokens = this.tokens;

        this.log.forEach((el) => {
            if (el.uid) {
                tokens.series.push([0, eventNameToString(el)]);
                tokens.eventObjects[eventNameToString(el)] = {uid: el.uid, pattern: el.pattern};
            } else {
                tokens.series.push([1, String(el.name)]);
            }
          });
          
          tokens.series.forEach((t) => {
            if (t[0] > 0) {
              tokens.numbers[t[1]] = true;
            } else {
              tokens.letters[t[1]] = true;
            }
          });
          
          Object.keys(tokens.letters).forEach((t, i) => {
            tokens.letters[t] = letters.charAt(i);
            tokens.fromLetters[letters.charAt(i)] = t;
          });
          
          Object.keys(tokens.numbers).forEach((t, i) => {
            tokens.numbers[t] = numbers.charAt(i);
            tokens.fromDigits[numbers.charAt(i)] = t;
          });
          
          
          tokens.series = tokens.series.map((el) => {
            if (el[0] > 0) {
              return tokens.numbers[el[1]]
            } else {
              return tokens.letters[el[1]]
            }
          }).join('');

        return this;
    }

    analyze() {
        let groups = findPositions(this.tokens.series);
  
        groups = groups.map((g) => {
          g.database = new Map();
          g.whitelist = Object.keys(g.connections).map((e) => this.tokens.fromLetters[e]);
          return g;
        });

        this.groups = groups;
        return groups;
    }

    makeGroups(fullFormQ) {
        if (fullFormQ) {
            return this.groups.map((group) => {
              const tranformed = {
                  count: {},
                  probabilities: {},
                  connections: {},
                  structure: []
              };

              Object.keys(group.count).forEach((k) => {
                  tranformed.count[this.tokens.fromLetters[k]] =     group.count[k];    
              });

              Object.keys(group.probabilities).forEach((k) => {
                  tranformed.probabilities[this.tokens.fromLetters[k]] = {};
                  Object.keys(group.probabilities[k]).forEach((kk) => {
                      tranformed.probabilities[this.tokens.fromLetters[k]][this.tokens.fromDigits[kk]] = group.probabilities[k][kk]
                  });
              });

              Object.keys(group.connections).forEach((k) => {
                  tranformed.connections[this.tokens.fromLetters[k]] = group.connections[k].map((el) => this.tokens.fromDigits[el]);
              });

           

              tranformed.structure = group.structure.map((element) => {
                const type = this.tokens.fromLetters[element.type];
                return {
                  ...element,
                  type: type,
                  elements : element.elements.map((el) => {
                    return {
                      ...el,
                      data: this.tokens.fromDigits[el.data]
                    }
                  })
                };
              });

              tranformed.eventObjects = filterKeys(this.tokens.eventObjects, (e) => (e in tranformed.connections));

              return tranformed;

          });
        }

        return this.groups.map((group) => {
            const tranformed = {
                count: {},
                probabilities: {},
                connections: {}
            };

            Object.keys(group.count).forEach((k) => {
                tranformed.count[this.tokens.fromLetters[k]] =     group.count[k];    
            });

            Object.keys(group.probabilities).forEach((k) => {
                tranformed.probabilities[this.tokens.fromLetters[k]] = {};
                Object.keys(group.probabilities[k]).forEach((kk) => {
                    tranformed.probabilities[this.tokens.fromLetters[k]][this.tokens.fromDigits[kk]] = group.probabilities[k][kk]
                });
            });

            Object.keys(group.connections).forEach((k) => {
                tranformed.connections[this.tokens.fromLetters[k]] = group.connections[k].map((el) => this.tokens.fromDigits[el]);
            });




            
            tranformed.eventObjects = filterKeys(this.tokens.eventObjects, (e) => (e in tranformed.connections));
            
            return tranformed;

        });
    }


}

  

  
  
  

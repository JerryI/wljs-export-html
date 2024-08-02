import { A as AnalyzerNode, e as eventNameToString } from './analyzer-4cf83c5b.js';

function memorySizeOf(obj) {
  var bytes = 0;

  function sizeOf(obj) {
    if (obj !== null && obj !== undefined) {
      switch (typeof obj) {
        case "number":
          bytes += 8;
          break;
        case "string":
          bytes += obj.length * 2;
          break;
        case "boolean":
          bytes += 4;
          break;
        case "object":
          var objClass = Object.prototype.toString.call(obj).slice(8, -1);
          if (objClass === "Object" || objClass === "Array") {
            for (var key in obj) {
              if (!obj.hasOwnProperty(key)) continue;
              sizeOf(obj[key]);
            }
          } else bytes += obj.toString().length * 2;
          break;
      }
    }
    return bytes;
  }
  return sizeOf(obj);
}

function countDuplicates(arr) {
  const seen = new Set();
  const duplicates = new Set();

  for (let i = 0; i < arr.length; i++) {
      const elem = arr[i];
      if (seen.has(elem)) {
          duplicates.add(elem);
      } else {
          seen.add(elem);
      }
  }

  return duplicates.size;
}

class SamplerNode {
  que = {}

  channel = ''
  emitter = () => {}

  recieve = ({name, data}) => {
    if (name in this.que) {
      this.que[name].resolve(data);
      delete this.que[name];
    } else {
      console.warn('Unknown symbol update!');
      console.warn({name, data});
    }
  };

  constructor(instance, channel) {
    this.channel = channel;
    this.instance = instance;

    const node = new AnalyzerNode(this.instance.dump);
    node.analyze();
    this.groups = node.makeGroups(true);

    server.emitt(this.channel, `<|"Info" -> "Ready", "Size" -> 0|>`, 'Progress'); 

    this.groups = this.groups.map((g) => {
      return {...g, eventObjects: this.process(g)};
    });

    this.groups.map(this.checkAnimations);

    //purge extra material
    this.groups.forEach((g) => delete g.structure);
    delete this.instance;

    console.log(this.groups);

    return this;
  }

  process(group) {
    const log = this.instance.dump;
    const eventObjects = {};
    
    Object.keys(group.eventObjects).forEach((ev) => {
      eventObjects[ev] = {
        connections: group.connections[ev],
        ...group.eventObjects[ev],
        data: log.filter((o) => {
          if (o.uid) {
            if (eventNameToString(o) == ev) return true;
          }
          
          return false
        }).map((o) => o.data)
      };
      
      if (countDuplicates(eventObjects[ev].data) > 30) {
        eventObjects[ev].animation = true;
      }
      
        
    });

    return eventObjects;
  }

  checkAnimations(group) {
    Object.keys(group.eventObjects).forEach((code) => {
      const eventObject = group.eventObjects[code];

      if (!eventObject.animation) return;
      console.warn('Animation detected!');

      let size = 0;

      const frames = group.structure.filter((e) => (e.type === code)).map((frame) => {
        const elements = frame.elements;
        return elements.map((u) => {
          const i = this.instance.dump[u.pos];
          size += memorySizeOf(i.data);

          return i;
        });
      });

      eventObject.animation = frames;

      server.emitt(this.channel, `<|"Info" -> "Animation ${frames.length} frames", "Size" -> ${Math.round(size / 1024)}|>`, 'Progress'); 

      //server.emitt(this.channel, `<|"Bar" -> ${Math.round(0.0)}, "Max" -> 1.0, "Info" -> ${}, "Size" -> ${}|>`, 'Progress'); 

       
    });

  }

  async start() {
    for (let group of this.groups) {
      console.log('Sampling...');
      await this.sample(group);
    }
  }


  sample(group) {
    let totalPoints = 0;
    let individualPoints = [];
    const list = Object.values(group.eventObjects).filter((o) => (!o.animation));

    list.forEach((e) => {
      if (totalPoints < 1) totalPoints = e.data.length; else totalPoints *= e.data.length;
      individualPoints.push(e.data.length);
    });

    server.emitt(this.channel, `<|"Info" -> "Sampling ${totalPoints} points", "Max" -> ${totalPoints}, "Bar" -> 0.0|>`, 'Progress'); 


    //singles considering undefined initial state
    //reset all to possible initial state
    console.warn('Reset to initial state!');

    list.forEach((e) => {
      this.emitter({uid: e.uid, data: e.data[0], pattern: e.pattern});
    });

    
    const promise = new Deferred();
    //que.push(promise);
    return promise;
  }

}

export { SamplerNode };

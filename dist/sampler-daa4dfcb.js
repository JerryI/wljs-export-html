import { e as eventNameToString, A as AnalyzerNode } from './analyzer-67a69c32.js';

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
  que = []

  channel = ''
  emitter = () => {}
  reciver () {

  }

  constructor(instance, channel) {
    this.channel = channel;
    this.instance = instance;

    return this;
  }

  process(group, log) {
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

  start() {
    const node = new AnalyzerNode(this.instance.dump);
    node.analyze();
    this.groups = node.makeGroups(true);

    this.groups = this.groups.map((g) => {
      return {...g, eventObjects: this.process(eventObjects)};
    });

    this.groups.map(this.checkAnimations);

    console.log(this.groups);
  }



}

export { SamplerNode };

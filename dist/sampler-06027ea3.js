import { AnalyzerNode } from './analyzer-435e32db.js';

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

  animationFramesQ() {

  }

  start() {
    const node = new AnalyzerNode(this.instance.dump);
    node.analyze();
    this.groups = node.makeGroups();

    console.log(this.groups);
    console.log(this.instance.dump);

    if (animationFramesQ()) {
      server.emitt(channel, 'Animation detected!', 'Message');
      return;
    }
  }



}

export { SamplerNode };

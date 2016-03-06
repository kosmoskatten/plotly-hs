import React from 'react';
import PlotSelector from './PlotSelector.jsx';
import PlotGrid from './PlotGrid.jsx';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {plots: [], nextSeq: 1};
  }

  render() {
    return (
      <div className='w3-container w3-row'>
        <PlotSelector addPlot={this.addPlot.bind(this)} />
        <PlotGrid removePlot={this.removePlot.bind(this)} 
                  plots={this.state.plots} />
      </div>
    );
  }

  // Callback from the PlotSelector with an entry to the selected plot.
  addPlot(entry) {
    console.log('addPlot: ' + entry.link);
    let newPlot = {entry: entry, seq: this.state.nextSeq};
    this.setState({plots: this.state.plots.concat([newPlot]),
                   nextSeq: this.state.nextSeq + 1});
  }

  // Callback from PlotGrid with a seq number to remove a plot.
  removePlot(seq) {
    console.log('App.removePlot: ' + seq);
    this.setState({plots: this.state.plots.filter(plot => {
      return plot.seq != seq;
    }), nextSeq: this.state.nextSeq});
  }
}

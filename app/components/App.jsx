import React from 'react';
import PlotSelector from './PlotSelector.jsx';
import PlotGrid from './PlotGrid.jsx';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {plots: []};
  }

  render() {
    return (
      <div className='w3-container w3-row'>
        <PlotSelector addPlot={this.addPlot.bind(this)} />
        <PlotGrid plots={this.state.plots} />
      </div>
    );
  }

  // Callback from the PlotSelector with an entry to the selected plot.
  addPlot(entry) {
    console.log("addPlot: " + entry.link);
    this.setState({plots: this.state.plots.concat([entry])});
  }
}

import React from 'react';
import PlotSelector from './PlotSelector.jsx';

export default class App extends React.Component {
  render() {
    return <PlotSelector addPlot={this.addPlot.bind(this)}/>;
  }

  // Callback from the PlotSelector with a link to the selected plot.
  addPlot(link) {
    console.log("Got link: " + link);
  }
}

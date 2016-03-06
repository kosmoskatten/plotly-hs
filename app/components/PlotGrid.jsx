import React from 'react';
import Plot from './Plot.jsx';

/*
 * The PlotGrid displays a grid of selected plots. The plots to display
 * are provided as a property list with url links to the plots.
 */
export default class PlotGrid extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
        <div className='w3-container w3-col m8 l8'>
          {this.props.plots.map((plot, i) => {
            return <Plot key={i}
                    removePlot={this.removePlot.bind(this)}
                    plot={plot} />
          })}
        </div>
    );
  }

  // A plot card has requested to close.
  removePlot(seq) {
    console.log('PlotGrid.removePlot: ' + seq);

    // Propagate the event to App.
    this.props.removePlot(seq);
  }
};

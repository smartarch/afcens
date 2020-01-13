'use strict';

import React, {Component} from "react";
import {withComponentMixins} from "../../ivis-core/client/src/lib/decorator-helpers";
import {withTranslation} from "../../ivis-core/client/src/lib/i18n";
import {withAsyncErrorHandler, withErrorHandling} from "../../ivis-core/client/src/lib/error-handling";
import {withPageHelpers} from "../../ivis-core/client/src/lib/page-common";
import {requiresAuthenticatedUser} from "../../ivis-core/client/src/lib/page";
import {Panel} from "../../ivis-core/client/src/lib/panel";
import {Button} from "../../ivis-core/client/src/lib/bootstrap-components";
import styles from "./ScenarioMap.scss";
import axios from "../../ivis-core/client/src/lib/axios";
import {getUrl} from "../../ivis-core/client/src/lib/urls";
import moment from "moment";
import {SVG} from "../../ivis-core/client/src/ivis/SVG";
import scenarioMapSvg from "../images/fields-symbols.svg";
import { select, event as d3Event, mouse } from 'd3-selection';

const State = {
    START: 0,
    PLAYING: 1,
    PAUSED: 2,
    END: 3
};

const AgentType = {
    DRONE: 0,
    FLOCK: 1
};

const refreshInterval = 50;
const minorStepsInRefetchPeriod = 5;

@withComponentMixins([
    withTranslation,
    withErrorHandling,
    withPageHelpers,
    requiresAuthenticatedUser
])export default class ScenarioMap extends Component {
    constructor(props) {
        super(props);

        this.state = {
            playState: State.START,
            ts: null,
            agents: [],
            selectedAgent: null
        };

        this.reset();

        this.refreshIntervalId = null;

        this.agentTypeLabels = {
            [AgentType.DRONE]: t('Drone'),
            [AgentType.FLOCK]: t('Flock'),
        }
    }

    reset() {
        this.keyframes = [];
        this.minorStep = 0;
        this.initialKeyframesFetched = false;
    }

    @withAsyncErrorHandler
    async play() {
        await axios.post(getUrl('sim/play'));

        this.setState({
            playState: State.PLAYING
        });
    }

    @withAsyncErrorHandler
    async stop() {
        await axios.post(getUrl('sim/reset'));

        this.setState({
            playState: State.START,
            ts: null
        });

        this.reset();
    }

    @withAsyncErrorHandler
    async pause() {
        await axios.post(getUrl('sim/pause'));

        this.setState({
            playState: State.PAUSED
        });
    }

    @withAsyncErrorHandler
    async getStatus() {
        const resp = await axios.get(getUrl('sim/status'));
        const ts = moment(resp.data.time);
        const kf = this.keyframes;

        if (kf.length > 0 && kf[kf.length - 1].ts.isAfter(ts)) {
            // This may happen if we receive status from previous epoch after stop. The status from the previous
            // epoch will set ts to something high. The reset status that we receive afterwards won't match
            // the last ts, which is what we check here.
            this.reset();

        } else {
            const agents = {};
            for (const droneKey in resp.data.drones) {
                const drone = resp.data.drones[droneKey];
                agents[droneKey] = {
                    type: AgentType.DRONE,
                    position: drone.position
                };
            }

            for (const flockKey in resp.data.flocks) {
                const flock = resp.data.drones[flockKey];
                agents[flockKey] = {
                    type: AgentType.FLOCK,
                    position: flock.position
                };
            }

            kf.push({
                ts,
                agents,
                ensembles: resp.data.ensembles
            });

            const playState = resp.data.playState;
            if (this.state.playState !== playState) {
                this.setState({
                    playState
                });
            }

            if (kf.length === 3 && !this.initialKeyframesFetched) {
                this.initialKeyframesFetched = true;
                this.minorStep = 0;
            }
        }
    }

    refresh() {
        if (!this.initialKeyframesFetched) {
            this.minorStep += 1;
            if (this.minorStep === minorStepsInRefetchPeriod) {
                this.minorStep = 0;
                // noinspection JSIgnoredPromiseFromCall
                this.getStatus();
            }

        } else {
            const kf = this.keyframes;

            if (kf.length >= 2) {
                const last = kf[0];
                const next = kf[1];
                const alpha = this.minorStep / minorStepsInRefetchPeriod;

                const interp = (last, next) => last * (1-alpha) + next * alpha;

                const ts = moment(interp(last.ts.valueOf(), next.ts.valueOf()));

                if (!this.state.ts || ts.valueOf() !== this.state.ts.valueOf()) {

                    const agents = [];
                    for (const key in last.agents) {
                        if (key in next.agents) {
                            const lastAgent = last.agents[key];
                            const nextAgent = next.agents[key];
                            
                            const type = lastAgent.type;

                            let symbol;
                            if (type === AgentType.DRONE) {
                                symbol = 'Drone';
                            } else if (type === AgentType.FLOCK) {
                                symbol = 'Bird';
                            }

                            agents.push({
                                id: key,
                                type,
                                symbol,
                                x: interp(lastAgent.position.x, nextAgent.position.x),
                                y: interp(lastAgent.position.y, nextAgent.position.y),
                            });
                        }
                    }


                    this.setState({
                        ts,
                        agents,
                        ensembles: last.ensembles
                    });
                }
            }

            this.minorStep += 1;
            if (this.minorStep === minorStepsInRefetchPeriod) {
                this.minorStep = 0;

                if (kf.length >= 2) {
                    kf.shift();
                }

                // noinspection JSIgnoredPromiseFromCall
                this.getStatus();
            }
        }
    }

    componentDidMount() {
        this.getStatus();
        this.refreshIntervalId = setInterval(::this.refresh, refreshInterval);
    }

    componentWillUnmount() {
        clearInterval(this.refreshIntervalId);
    }

    render() {
        const t = this.props.t;
        const playState = this.state.playState;

        const ts = this.state.ts;
        const tsFormatted = ts ? ts.utc().format('HH:mm:ss') : null;

        const selAgentId = this.state.selectedAgent;
        let agentDetails;
        if (selAgentId) {
            let selAgent = this.state.agents.find(agent => agent.id === selAgentId);

            if (selAgent) {
                agentDetails = (
                    <>
                        <div className={`card-body ${styles.detailsSection}`}>
                            <div className={`card-title ${styles.detailsSectionHeader}`}>{t('Identification')}</div>
                            <div className="card-text">
                                {this.agentTypeLabels[selAgent.type]}: {selAgentId}
                            </div>
                        </div>
                    </>
                );

            } else {
                agentDetails = (
                    <>
                        <div className={`card-body ${styles.detailsSection}`}>
                            <div className={`card-title ${styles.detailsSectionHeader}`}>{t('Identification')}</div>
                            <div className="card-text">
                                {selAgentId} ({t('loading further info')})
                            </div>
                        </div>
                    </>
                );
            }

        } else {
            agentDetails = (
                <div className={`card-body ${styles.detailsSection}`}>
                    <div className="card-text">
                        <div className={styles.help}>{t('Select an agent to see details')}</div>
                    </div>
                </div>
            );
        }

        return (
            <Panel title={t('Scenario')}>
                <div className="row">
                    <div className="col-12 col-lg-9">
                        <div className="row">
                            <div className="col-12 col-lg-9 mb-3">
                                { playState === State.PLAYING &&
                                    <Button className={`btn-primary ${styles.controlButton}`} icon="pause" onClickAsync={::this.pause} />
                                }
                                { (playState === State.START || playState === State.PAUSED) &&
                                <Button className={`btn-primary ${styles.controlButton}`} icon="play" onClickAsync={::this.play} />
                                }
                                { (playState === State.END) &&
                                <Button className={`btn-primary ${styles.controlButton}`} icon="play" disabled={true} />
                                }
                                <Button className={`btn-danger ${styles.controlButton}`} icon="stop" onClickAsync={::this.stop} disabled={playState === State.START}/>
                                <span className={styles.timestamp}>{tsFormatted}</span>
                            </div>
                        </div>
                        <div>
                            <SVG
                                source={scenarioMapSvg}
                                init={node => {
                                    const viewBox = node.viewBox.baseVal;
                                    const nodeSel = select(node);
                                    const agentSizeHalf = 8.66 / 2; // FIXME

                                    nodeSel
                                        .append('rect')
                                        .attr('x', viewBox.x)
                                        .attr('y', viewBox.y)
                                        .attr('width', viewBox.width)
                                        .attr('height', viewBox.height)
                                        .attr('fill-opacity', 0)
                                        .attr('stroke', 'none')
                                        .on("click", d => {
                                            const mousePos = mouse(node);
                                            const x = mousePos[0];
                                            const y = mousePos[1];
                                            let selectedAgent = null;
                                            for (const agent of this.state.agents) {
                                                if (agent.x - agentSizeHalf <= x && agent.x + agentSizeHalf >= x && agent.y - agentSizeHalf <= y && agent.y + agentSizeHalf >= y) {
                                                    selectedAgent = agent.id;
                                                }
                                            }
                                            this.setState({selectedAgent});
                                        });

                                    nodeSel
                                        .select('#Positions')
                                        .attr('display', 'none');
                                }}
                                data={{
                                    Agents: node => {
                                        const agents = this.state.agents;

                                        node.selectAll('use')
                                            .data(agents, d => d.id)
                                            .enter()
                                            .append('use');

                                        node.selectAll('use')
                                            .data(agents, d => d.id)
                                            .exit()
                                            .remove();

                                        node.selectAll('use')
                                            .data(agents, d => d.id)
                                            .attr('href', d => (d.id === this.state.selectedAgent ? "#" : "#") + d.symbol) // FIXME "#selected-"
                                            .attr('x', d => d.x - 10)
                                            .attr('y', d => d.y - 10);
                                    }
                                }}
                            />
                        </div>
                    </div>
                    <div className="col-12 col-lg-3">
                        <div className="card">
                            <div className="card-header">
                                <h4 className={styles.detailsHeader}>{t('Agent Details')}</h4>
                            </div>
                            {agentDetails}
                        </div>
                    </div>
                </div>
            </Panel>
        );
    }
}
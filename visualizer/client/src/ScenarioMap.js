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
import { rgb } from 'd3-color';
import PropTypes from "prop-types";
import * as d3Format from "d3-format";

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

const agentSymbolOffsets = {
    [AgentType.DRONE]: {x: -9.5, y: -2.5},
    [AgentType.FLOCK]: {x: -6, y: -6}
};

const agentVisibilityRadiuses = {
    [AgentType.DRONE]: {r: 50, color: rgb(3, 182, 252)},
    [AgentType.FLOCK]: {r: 50, color: rgb(252, 194, 3)}
};

const flockDisturbRadius = {
    r: 15,
    color: rgb(252, 98, 3)
};

const flockObservedRadius = {
    r: 15 - 6 /* 6 is the radius of the flock */,
    color: rgb(24, 64, 24)
};

const agentSymbolSizeHalf = 5;

const refreshInterval = 50;
const minorStepsInRefetchPeriod = 5;

@withComponentMixins([
    withTranslation,
    withErrorHandling,
    withPageHelpers,
    requiresAuthenticatedUser
])
export default class ScenarioMap extends Component {
    constructor(props) {
        super(props);

        const t = props.t;

        this.state = {
            playState: State.START,
            ts: null,
            agents: [],
            selectedAgent: null,
            visibilityRadiusVisible: false,
            observedForSelectedVisible: false
        };

        this.reset();

        this.refreshIntervalId = null;

        this.agentTypeLabels = {
            [AgentType.DRONE]: t('Drone'),
            [AgentType.FLOCK]: t('Flock'),
        };

        this.droneModeLabels = {
            0: t('Charging'),
            1: t('Moving'),
            2: t('Resting'),
            3: t('Dead'),
            4: t('Idle')
        };

        this.flockModeLabels = {
            0: t('Flying to rest'),
            1: t('Flying to eat'),
            2: t('Eating'),
            3: t('Resting'),
            4: t('Idle')
        };
    }

    static propTypes = {
        simulationId: PropTypes.string
    }

    reset() {
        this.keyframes = [];
        this.minorStep = 0;
        this.initialKeyframesFetched = false;
    }

    @withAsyncErrorHandler
    async play() {
        await axios.post(getUrl(`sim/${this.props.simulationId}/play`));

        this.setState({
            playState: State.PLAYING
        });
    }

    @withAsyncErrorHandler
    async stop() {
        await axios.post(getUrl(`sim/${this.props.simulationId}/reset`));

        this.setState({
            playState: State.START,
            ts: null
        });

        this.reset();
    }

    @withAsyncErrorHandler
    async pause() {
        await axios.post(getUrl(`sim/${this.props.simulationId}/pause`));

        this.setState({
            playState: State.PAUSED
        });
    }

    @withAsyncErrorHandler
    async getStatus() {
        const resp = await axios.get(getUrl(`sim/${this.props.simulationId}/status`));
        const ts = moment(resp.data.time);
        const kf = this.keyframes;

        if (kf.length > 0 && kf[kf.length - 1].ts.isAfter(ts)) {
            // This may happen if we receive status from previous epoch after stop. The status from the previous
            // epoch will set ts to something high. The reset status that we receive afterwards won't match
            // the last ts, which is what we check here.
            this.reset();

        } else {
            console.log(resp.data.tasks);

            const agents = {};
            for (const droneKey in resp.data.drones) {
                const drone = resp.data.drones[droneKey];
                agents[droneKey] = {
                    type: AgentType.DRONE,
                    position: drone.position,
                    mode: drone.mode,
                    energy: drone.energy,
                    chargingInChargerId: drone.chargingInChargerId
                };
            }

            for (const flockKey in resp.data.flocks) {
                const flock = resp.data.flocks[flockKey];
                agents[flockKey] = {
                    type: AgentType.FLOCK,
                    position: flock.position,
                    mode: flock.mode,
                    observedDrones: flock.observedDrones,
                    eatTicks: flock.eatTicks
                };
            }

            kf.push({
                ts,
                eatTicks: resp.data.eatTicks,
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

                            const frame = {
                                id: key,
                                type,
                                x: interp(lastAgent.position.x, nextAgent.position.x),
                                y: interp(lastAgent.position.y, nextAgent.position.y),
                            };

                            if (type === AgentType.DRONE) {
                                frame.symbol = 'Drone';
                                frame.mode = lastAgent.mode;
                                frame.energy = interp(lastAgent.energy, nextAgent.energy);
                                frame.chargingInChargerId = lastAgent.chargingInChargerId;

                            } else if (type === AgentType.FLOCK) {
                                frame.symbol = 'Bird';
                                frame.mode = lastAgent.mode;
                                frame.observedDrones = lastAgent.observedDrones;
                                frame.eatTicks = interp(lastAgent.eatTicks, nextAgent.eatTicks)
                            }

                            agents.push(frame);
                        }
                    }


                    this.setState({
                        ts,
                        eatTicks: interp(last.eatTicks, next.eatTicks),
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
        const energyF = d3Format.format(".0f");
        const eatTicksF = d3Format.format(".1f");

        const playState = this.state.playState;

        const ts = this.state.ts;
        const tsFormatted = ts ? ts.utc().format('HH:mm:ss') : null;

        const selAgentId = this.state.selectedAgent;
        let agentDetails;
        let selAgent;
        if (selAgentId) {
            selAgent = this.state.agents.find(agent => agent.id === selAgentId);

            if (selAgent) {
                agentDetails = [
                    <div key="common" className={`card-body ${styles.detailsSection}`}>
                        <div className={`card-title ${styles.detailsSectionHeader}`}>{t('Identification')}</div>
                        <div className="card-text">
                            {selAgentId}
                        </div>
                    </div>
                ];

                if (selAgent.type === AgentType.DRONE) {
                    agentDetails.push(
                        <div key="droneDetails" className={`card-body ${styles.detailsSection}`}>
                            <div className={`card-title ${styles.detailsSectionHeader}`}>{t('Drone status')}</div>
                            <div className="card-text">
                                <div>{t('Mode')}: {this.droneModeLabels[selAgent.mode]}</div>
                                <div>{t('Energy')}: {energyF(selAgent.energy * 100)}%</div>
                            </div>
                        </div>
                    )
                }

                if (selAgent.type === AgentType.FLOCK) {
                    agentDetails.push(
                        <div key="flockDetails" className={`card-body ${styles.detailsSection}`}>
                            <div className={`card-title ${styles.detailsSectionHeader}`}>{t('Flock status')}</div>
                            <div className="card-text">
                                <div>{t('Mode')}: {this.flockModeLabels[selAgent.mode]}</div>
                                <div>{t('Eat ticks')}: {eatTicksF(selAgent.eatTicks)}</div>
                            </div>
                        </div>
                    )
                }

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
                                <span className={styles.eatTicks}>Total eaten: {eatTicksF(this.state.eatTicks)}</span>
                            </div>
                            <div className="col-12 col-lg-3 mb-3 text-lg-right">
                                <Button className={`btn-info ${styles.controlButton}`} icon="bullseye" onClickAsync={async () => this.setState({visibilityRadiusVisible: !this.state.visibilityRadiusVisible})} />
                                {selAgent && selAgent.type === AgentType.FLOCK && <Button className={`btn-info ${styles.controlButton}`} icon="eye" onClickAsync={async () => this.setState({observedForSelectedVisible: !this.state.observedForSelectedVisible})} /> }
                            </div>
                        </div>
                        <div>
                            <SVG
                                source={scenarioMapSvg}
                                init={node => {
                                    const viewBox = node.viewBox.baseVal;
                                    const nodeSel = select(node);

                                    nodeSel
                                        .select('#Positions')
                                        .attr('display', 'none');

                                    nodeSel
                                        .append('g')
                                        .attr('id', 'Agents');

                                    nodeSel
                                        .append('g')
                                        .attr('id', 'VisibilityRadiuses');

                                    nodeSel
                                        .append('g')
                                        .attr('id', 'FlockDisturbRadiuses');

                                    nodeSel
                                        .append('g')
                                        .attr('id', 'FlockObservations')
                                        .attr('opacity', 0.4);

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
                                                if (agent.x - agentSymbolSizeHalf <= x && agent.x + agentSymbolSizeHalf >= x && agent.y - agentSymbolSizeHalf <= y && agent.y + agentSymbolSizeHalf >= y) {
                                                    selectedAgent = agent.id;
                                                }
                                            }
                                            this.setState({selectedAgent});
                                        });
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
                                            .attr('x', d => d.x + agentSymbolOffsets[d.type].x)
                                            .attr('y', d => d.y + agentSymbolOffsets[d.type].y);
                                    },
                                    VisibilityRadiuses: node => {
                                        const agents = this.state.visibilityRadiusVisible ? this.state.agents : [];

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .enter()
                                            .append('circle');

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .exit()
                                            .remove();

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .attr('cx', d => d.x)
                                            .attr('cy', d => d.y)
                                            .attr('r', d => agentVisibilityRadiuses[d.type].r)
                                            .attr('stroke', d => agentVisibilityRadiuses[d.type].color)
                                            .attr('stroke-width', 1)
                                            .attr('fill', 'none');
                                    },
                                    FlockDisturbRadiuses: node => {
                                        const agents = this.state.visibilityRadiusVisible ? this.state.agents.filter(x => x.type === AgentType.FLOCK) : [];

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .enter()
                                            .append('circle');

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .exit()
                                            .remove();

                                        node.selectAll('circle')
                                            .data(agents, d => d.id)
                                            .attr('cx', d => d.x)
                                            .attr('cy', d => d.y)
                                            .attr('r', flockDisturbRadius.r)
                                            .attr('stroke', flockDisturbRadius.color)
                                            .attr('stroke-width', 1)
                                            .attr('fill', 'none');
                                    },
                                    FlockObservations: node => {
                                        let spots = [];

                                        if (selAgent && selAgent.type === AgentType.FLOCK && this.state.observedForSelectedVisible) {
                                            spots = selAgent.observedDrones.map(pos => ({id: `${pos.x}-${pos.y}`, x: pos.x, y: pos.y}));
                                        }

                                        node.selectAll('circle')
                                            .data(spots, d => d.id)
                                            .enter()
                                            .append('circle');

                                        node.selectAll('circle')
                                            .data(spots, d => d.id)
                                            .exit()
                                            .remove();

                                        node.selectAll('circle')
                                            .data(spots, d => d.id)
                                            .attr('cx', d => d.x)
                                            .attr('cy', d => d.y)
                                            .attr('r', flockObservedRadius.r)
                                            .attr('fill', flockObservedRadius.color)
                                            .attr('stroke', 'none');
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
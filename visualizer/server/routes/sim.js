'use strict';

const router = require('../../ivis-core/server/lib/router-async').create();
const log = require('../../ivis-core/server/lib/log');
const passport = require('../../ivis-core/server/lib/passport');
const axios = require('axios');
const config = require('../../ivis-core/server/lib/config');

function getSimulatorUrl(path) {
    return config.simulator.url + path;
}

router.postAsync('/:simulationId/play', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl(req.params.simulationId + '/play'));
    return res.json();
});

router.postAsync('/:simulationId/pause', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl(req.params.simulationId + '/pause'));
    return res.json();
});

router.postAsync('/:simulationId/reset', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl(req.params.simulationId + '/reset'));
    return res.json();
});

router.getAsync('/:simulationId/status', passport.loggedIn, async (req, res) => {
    const resp = await axios.get(getSimulatorUrl(req.params.simulationId + '/status'));
    return res.json(resp.data);
});

module.exports = router;

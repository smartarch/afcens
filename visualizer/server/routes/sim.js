'use strict';

const router = require('../../ivis-core/server/lib/router-async').create();
const log = require('../../ivis-core/server/lib/log');
const passport = require('../../ivis-core/server/lib/passport');
const axios = require('axios');
const config = require('../../ivis-core/server/lib/config');

function getSimulatorUrl(path) {
    return config.simulator.url + path;
}

router.postAsync('/play', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl('play'));
    return res.json();
});

router.postAsync('/pause', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl('pause'));
    return res.json();
});

router.postAsync('/reset', passport.loggedIn, passport.csrfProtection, async (req, res) => {
    const resp = await axios.post(getSimulatorUrl('reset'));
    return res.json();
});

router.getAsync('/status', passport.loggedIn, async (req, res) => {
    const resp = await axios.get(getSimulatorUrl('status'));
    return res.json(resp.data);
});

module.exports = router;

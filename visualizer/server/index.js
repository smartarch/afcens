'use strict';

require('./extensions-common');

const em = require('../ivis-core/server/lib/extension-manager');
const path = require('path');
const { AppType } = require('../ivis-core/shared/app');

em.set('app.clientDist', path.join(__dirname, '..', 'client', 'dist'));

em.on('knex.migrate', async () => {
    const knex = require('../ivis-core/server/lib/knex');
    await knex.migrateExtension('afcens', './knex/migrations').latest();
});

em.on('app.installRoutes', app => {
    const sim = require('./routes/sim');
    app.use('/sim', sim);
});

em.on('server.setup', (server, app, appType) => {
    if (appType === AppType.API) {
        server.keepAliveTimeout = 0;
    }
});

require('../ivis-core/server/index');


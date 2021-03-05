'use strict';

import React from "react";

import em from '../../ivis-core/client/src/lib/extension-manager';
import ScenarioMap from './ScenarioMap';
import {NavLink} from "../../ivis-core/client/src/lib/page";

import ivisConfig from "ivisConfig";

em.set('app.title', 'AFCEns Demo');

const predictorIds = ['standalone', 'ensembles', 'dt_10k_single', 'dt_100k_single', 'dt_500k_single', 'dt_10k_multiple', 'dt_100k_multiple', 'dt_500k_multiple', 'nn_10k', 'nn_100k'];

em.on('client.installRoutes', (structure, t) => {
    const workspacesChildren = structure.children['workspaces'].children;

    for (const predictorId of predictorIds) {
        workspacesChildren[predictorId] = {
            title: t(predictorId),
            link: '/workspaces/' + predictorId,
            panelRender: () => <ScenarioMap simulationId={predictorId} />,
            secondaryMenuComponent: null
        };

    }

    structure.link = () => ivisConfig.isAuthenticated ? '/workspaces' : '/login';
});

em.on('client.mainMenuAuthenticated.installWorkspaces', (workspaces, t) => {
    for (const predictorId of predictorIds) {
        workspaces.push(<NavLink key={predictorId} to={'/workspaces/' + predictorId}>{t(predictorId)}</NavLink>);
    }
});

require('../../ivis-core/client/src/root-trusted');


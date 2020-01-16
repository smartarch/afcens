'use strict';

import React from "react";

import em from '../../ivis-core/client/src/lib/extension-manager';
import ScenarioMap from './ScenarioMap';
import {NavLink} from "../../ivis-core/client/src/lib/page";

import ivisConfig from "ivisConfig";

em.set('app.title', 'AFCEns Demo');

em.on('client.installRoutes', (structure, t) => {
    structure.children['workspaces'].children['scenario-map'] = {
        title: t('Scenario Map'),
        link: '/workspaces/scenario-map',
        panelComponent: ScenarioMap,
        secondaryMenuComponent: null
    };

    structure.link = () => ivisConfig.isAuthenticated ? '/workspaces' : '/login';
});

em.on('client.mainMenuAuthenticated.installWorkspaces', (workspaces, t) => {
    workspaces.push(<NavLink key="scenario-map" to="/workspaces/scenario-map">{t('Scenario Map')}</NavLink>);
});

require('../../ivis-core/client/src/root-trusted');


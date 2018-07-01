import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
// import playerImage from './player.png';
// import fallingObjectImage from './falling_object.png';

const localStorageKey = 'wolfadex-dodge-highScore';
const app = Main.embed(document.getElementById('root'));
const getLocalScores = () => {
    const highScores = window.localStorage.getItem(localStorageKey);

    try {
        return JSON.parse(highScores) || [];
    } catch (e) {
        return [];
    }
};

window.addEventListener('blur', () => {
    app.ports.windowBlur.send('');
});

app.ports.saveScore.subscribe(([player, score]) => {
    if (window.localStorage) {
        const parsedHighScores = getLocalScores();
        const newHighScores = [ ...parsedHighScores, { player, score }];

        newHighScores.sort((a, b) => {
            if (a.score < b.score) {
                return 1;
            }

            return -1;
        });

        const top10Scores = newHighScores.slice(0, 10);

        window.localStorage.setItem(localStorageKey, JSON.stringify(top10Scores));
        app.ports.receiveScores.send(top10Scores);
    }
});

app.ports.getScores.subscribe(() => {
    app.ports.receiveScores.send(getLocalScores());
});

registerServiceWorker();


yarn build
rsync -avz --delete dist/ decoded@m.carna.io:/home/decoded/public_html/m.carna.io/
